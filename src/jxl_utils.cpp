#include <Rcpp.h>
#include <jxl/decode.h>
#include <jxl/encode.h>
#include <jxl/encode_cxx.h>
#include <jxl/thread_parallel_runner.h>
#include <jxl/thread_parallel_runner_cxx.h>

#include <fstream>
#include <memory>
#include <vector>

using namespace Rcpp;

// Simple deleters for RAII
struct JxlDecoderDeleter {
  void operator()(JxlDecoder* d) {
    if (d) JxlDecoderDestroy(d);
  }
};
using JxlDecoderPtr = std::unique_ptr<JxlDecoder, JxlDecoderDeleter>;

// Helper to check JXL results and throw R errors
void check_status(JxlDecoderStatus status, const char* msg) {
  if (status != JXL_DEC_SUCCESS) {
    stop(std::string(msg) + " (status: " + std::to_string(status) + ")");
  }
}

void check_status(JxlEncoderStatus status, const char* msg) {
  if (status != JXL_ENC_SUCCESS) {
    stop(std::string(msg) + " (status: " + std::to_string(status) + ")");
  }
}

// Simple file reader
std::vector<uint8_t> read_file(const std::string& filename) {
  std::ifstream file(filename, std::ios::binary | std::ios::ate);
  if (!file.is_open()) stop("Cannot open file: " + filename);

  const auto size = file.tellg();
  file.seekg(0, std::ios::beg);

  std::vector<uint8_t> data(size);
  file.read(reinterpret_cast<char*>(data.data()), size);
  return data;
}

// Create basic info for encoding
JxlBasicInfo make_basic_info(uint32_t width, uint32_t height, uint32_t channels,
                             bool animation = false) {
  JxlBasicInfo info;
  JxlEncoderInitBasicInfo(&info);
  info.xsize = width;
  info.ysize = height;
  info.bits_per_sample = 8;
  info.num_color_channels = 3;
  info.alpha_bits = (channels == 4) ? 8 : 0;
  info.num_extra_channels = (channels == 4) ? 1 : 0;

  if (animation) {
    info.have_animation = JXL_TRUE;
    info.animation.tps_numerator = 1000;  // milliseconds
    info.animation.tps_denominator = 1;
  }

  return info;
}

// [[Rcpp::export]]
List jxl_info(const std::string& filename) {
  auto data = read_file(filename);

  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create decoder");

  check_status(JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO),
               "Failed to subscribe to events");
  check_status(JxlDecoderSetInput(decoder.get(), data.data(), data.size()),
               "Failed to set input");
  JxlDecoderCloseInput(decoder.get());

  if (JxlDecoderProcessInput(decoder.get()) != JXL_DEC_BASIC_INFO) {
    stop("Failed to decode basic info");
  }

  JxlBasicInfo info;
  check_status(JxlDecoderGetBasicInfo(decoder.get(), &info),
               "Failed to get basic info");

  return List::create(_["width"] = (int)info.xsize,
                      _["height"] = (int)info.ysize,
                      _["channels"] = (int)info.num_color_channels,
                      _["alpha"] = info.alpha_bits > 0,
                      _["bits_per_sample"] = (int)info.bits_per_sample,
                      _["uses_original_profile"] = info.uses_original_profile);
}

// [[Rcpp::export("R_jxl_get_info")]]
IntegerVector jxl_get_info_raw(const RawVector& buf) {
  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create decoder");

  check_status(JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO),
               "Failed to subscribe to events");
  check_status(JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
               "Failed to set input");
  JxlDecoderCloseInput(decoder.get());

  if (JxlDecoderProcessInput(decoder.get()) != JXL_DEC_BASIC_INFO) {
    stop("Failed to decode basic info");
  }

  JxlBasicInfo info;
  check_status(JxlDecoderGetBasicInfo(decoder.get(), &info),
               "Failed to get basic info");

  return IntegerVector::create((int)info.xsize, (int)info.ysize);
}

// [[Rcpp::export("R_jxl_decode")]]
RawVector jxl_decode_raw(const RawVector& buf) {
  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create decoder");

  check_status(JxlDecoderSubscribeEvents(
                   decoder.get(), JXL_DEC_BASIC_INFO | JXL_DEC_FULL_IMAGE),
               "Failed to subscribe to events");
  check_status(JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
               "Failed to set input");
  JxlDecoderCloseInput(decoder.get());

  JxlBasicInfo info;
  const JxlPixelFormat format = {4, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};

  while (true) {
    auto status = JxlDecoderProcessInput(decoder.get());

    if (status == JXL_DEC_ERROR || status == JXL_DEC_NEED_MORE_INPUT) {
      stop("Decoder error");
    } else if (status == JXL_DEC_BASIC_INFO) {
      check_status(JxlDecoderGetBasicInfo(decoder.get(), &info),
                   "Failed to get basic info");
    } else if (status == JXL_DEC_NEED_IMAGE_OUT_BUFFER) {
      size_t size;
      check_status(JxlDecoderImageOutBufferSize(decoder.get(), &format, &size),
                   "Failed to get buffer size");

      RawVector image(size);
      check_status(
          JxlDecoderSetImageOutBuffer(decoder.get(), &format, RAW(image), size),
          "Failed to set output buffer");

      image.attr("dim") =
          IntegerVector::create(4, (int)info.xsize, (int)info.ysize);
      return image;
    } else if (status == JXL_DEC_FULL_IMAGE) {
      continue;
    } else if (status == JXL_DEC_SUCCESS) {
      stop("Unexpected success without image");
    }
  }
}

// [[Rcpp::export("R_jxl_encode")]]
RawVector jxl_encode_raw(const RawVector& img, const IntegerVector& quality,
                         const IntegerVector& effort) {
  auto dim = as<IntegerVector>(img.attr("dim"));
  uint32_t channels = dim[0], width = dim[1], height = dim[2];

  // Create encoder
  auto encoder = JxlEncoderPtr(JxlEncoderMake(nullptr));
  auto runner = JxlThreadParallelRunnerPtr(JxlThreadParallelRunnerMake(
      nullptr, JxlThreadParallelRunnerDefaultNumWorkerThreads()));

  if (!encoder || !runner) stop("Failed to create encoder");

  check_status(JxlEncoderSetParallelRunner(
                   encoder.get(), JxlThreadParallelRunner, runner.get()),
               "Failed to set parallel runner");

  // Set basic info
  auto info = make_basic_info(width, height, channels);
  check_status(JxlEncoderSetBasicInfo(encoder.get(), &info),
               "Failed to set basic info");

  // Set color encoding
  JxlColorEncoding color = {};
  JxlColorEncodingSetToSRGB(&color, JXL_FALSE);
  check_status(JxlEncoderSetColorEncoding(encoder.get(), &color),
               "Failed to set color encoding");

  // Create frame settings
  auto* settings = JxlEncoderFrameSettingsCreate(encoder.get(), nullptr);
  if (!settings) stop("Failed to create frame settings");

  // Configure quality/effort
  int effort_val = (effort[0] == NA_INTEGER) ? 7 : effort[0];
  check_status(JxlEncoderFrameSettingsSetOption(
                   settings, JXL_ENC_FRAME_SETTING_EFFORT, effort_val),
               "Failed to set effort");

  if (quality[0] == NA_INTEGER) {
    check_status(JxlEncoderSetFrameDistance(settings, 0.0f),
                 "Failed to set lossless");
  } else {
    float distance =
        std::max(0.1f, std::min(15.0f, (100.0f - quality[0]) / 10.0f));
    check_status(JxlEncoderSetFrameDistance(settings, distance),
                 "Failed to set quality");
  }

  // Add image
  const JxlPixelFormat format = {channels, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN,
                                 0};
  check_status(
      JxlEncoderAddImageFrame(settings, &format, RAW(img), LENGTH(img)),
      "Failed to add image");

  JxlEncoderCloseInput(encoder.get());

  // Encode with dynamic buffer
  std::vector<uint8_t> buffer(1024);
  uint8_t* next_out = buffer.data();
  size_t avail_out = buffer.size();

  while (true) {
    auto status = JxlEncoderProcessOutput(encoder.get(), &next_out, &avail_out);

    if (status == JXL_ENC_SUCCESS) {
      buffer.resize(buffer.size() - avail_out);
      break;
    } else if (status == JXL_ENC_NEED_MORE_OUTPUT) {
      size_t offset = next_out - buffer.data();
      buffer.resize(buffer.size() * 2);
      next_out = buffer.data() + offset;
      avail_out = buffer.size() - offset;
    } else {
      stop("Encoding failed");
    }
  }

  RawVector out(buffer.size());
  std::copy(buffer.begin(), buffer.end(), RAW(out));
  return out;
}

// [[Rcpp::export("R_jxl_decode_anim")]]
List jxl_decode_anim_raw(const RawVector& buf) {
  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create decoder");

  check_status(JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO |
                                                            JXL_DEC_FRAME |
                                                            JXL_DEC_FULL_IMAGE),
               "Failed to subscribe to events");
  check_status(JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
               "Failed to set input");
  JxlDecoderCloseInput(decoder.get());

  JxlBasicInfo info;
  const JxlPixelFormat format = {4, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};
  std::vector<RawVector> frames;
  std::vector<int> durations;

  bool done = false;
  while (!done) {
    auto status = JxlDecoderProcessInput(decoder.get());

    switch (status) {
      case JXL_DEC_ERROR:
      case JXL_DEC_NEED_MORE_INPUT:
        stop("Decoder error");

      case JXL_DEC_BASIC_INFO:
        check_status(JxlDecoderGetBasicInfo(decoder.get(), &info),
                     "Failed to get basic info");
        break;

      case JXL_DEC_FRAME: {
        JxlFrameHeader header;
        check_status(JxlDecoderGetFrameHeader(decoder.get(), &header),
                     "Failed to get frame header");
        durations.push_back(
            (int)(header.duration * 1000.0f * info.animation.tps_denominator /
                  info.animation.tps_numerator));
        break;
      }

      case JXL_DEC_NEED_IMAGE_OUT_BUFFER: {
        size_t size;
        check_status(
            JxlDecoderImageOutBufferSize(decoder.get(), &format, &size),
            "Failed to get buffer size");
        RawVector image(size);
        check_status(JxlDecoderSetImageOutBuffer(decoder.get(), &format,
                                                 RAW(image), size),
                     "Failed to set output buffer");
        image.attr("dim") =
            IntegerVector::create(4, (int)info.xsize, (int)info.ysize);
        frames.push_back(image);
        break;
      }

      case JXL_DEC_FULL_IMAGE:
        continue;

      case JXL_DEC_SUCCESS:
        done = true;
        break;

      default:
        stop("Unknown decoder status");
    }
  }

  return List::create(_["frames"] = frames, _["durations"] = durations,
                      _["width"] = (int)info.xsize,
                      _["height"] = (int)info.ysize,
                      _["loop_count"] = (int)info.animation.num_loops);
}

// [[Rcpp::export("R_jxl_encode_anim")]]
RawVector jxl_encode_anim_raw(const List& frames,
                              const IntegerVector& durations,
                              const IntegerVector& quality,
                              const IntegerVector& effort,
                              const IntegerVector& loop_count) {
  if (frames.size() == 0) stop("No frames provided");

  auto first_frame = as<RawVector>(frames[0]);
  auto dim = as<IntegerVector>(first_frame.attr("dim"));
  uint32_t channels = dim[0], width = dim[1], height = dim[2];

  // Create encoder
  auto encoder = JxlEncoderPtr(JxlEncoderMake(nullptr));
  auto runner = JxlThreadParallelRunnerPtr(JxlThreadParallelRunnerMake(
      nullptr, JxlThreadParallelRunnerDefaultNumWorkerThreads()));

  if (!encoder || !runner) stop("Failed to create encoder");

  check_status(JxlEncoderSetParallelRunner(
                   encoder.get(), JxlThreadParallelRunner, runner.get()),
               "Failed to set parallel runner");

  // Set basic info with animation
  auto info = make_basic_info(width, height, channels, true);
  info.animation.num_loops = (loop_count[0] == NA_INTEGER) ? 0 : loop_count[0];
  check_status(JxlEncoderSetBasicInfo(encoder.get(), &info),
               "Failed to set basic info");

  // Set color encoding
  JxlColorEncoding color = {};
  JxlColorEncodingSetToSRGB(&color, JXL_FALSE);
  check_status(JxlEncoderSetColorEncoding(encoder.get(), &color),
               "Failed to set color encoding");

  // Quality/effort settings
  int effort_val = (effort[0] == NA_INTEGER) ? 7 : effort[0];
  float distance =
      (quality[0] == NA_INTEGER)
          ? 0.0f
          : std::max(0.1f, std::min(15.0f, (100.0f - quality[0]) / 10.0f));

  const JxlPixelFormat format = {channels, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN,
                                 0};

  // Add frames
  for (size_t i = 0; i < frames.size(); ++i) {
    auto* settings = JxlEncoderFrameSettingsCreate(encoder.get(), nullptr);
    if (!settings) stop("Failed to create frame settings");

    check_status(JxlEncoderFrameSettingsSetOption(
                     settings, JXL_ENC_FRAME_SETTING_EFFORT, effort_val),
                 "Failed to set effort");
    check_status(JxlEncoderSetFrameDistance(settings, distance),
                 "Failed to set quality");

    // Set frame header with duration
    JxlFrameHeader header;
    JxlEncoderInitFrameHeader(&header);
    header.duration = (i < (size_t)durations.size()) ? durations[i] : 100;
    check_status(JxlEncoderSetFrameHeader(settings, &header),
                 "Failed to set frame header");

    auto frame = as<RawVector>(frames[i]);
    check_status(
        JxlEncoderAddImageFrame(settings, &format, RAW(frame), LENGTH(frame)),
        "Failed to add frame");
  }

  JxlEncoderCloseInput(encoder.get());

  // Encode with dynamic buffer
  std::vector<uint8_t> buffer(1024);
  uint8_t* next_out = buffer.data();
  size_t avail_out = buffer.size();

  while (true) {
    auto status = JxlEncoderProcessOutput(encoder.get(), &next_out, &avail_out);

    if (status == JXL_ENC_SUCCESS) {
      buffer.resize(buffer.size() - avail_out);
      break;
    } else if (status == JXL_ENC_NEED_MORE_OUTPUT) {
      size_t offset = next_out - buffer.data();
      buffer.resize(buffer.size() * 2);
      next_out = buffer.data() + offset;
      avail_out = buffer.size() - offset;
    } else {
      stop("Encoding failed");
    }
  }

  RawVector out(buffer.size());
  std::copy(buffer.begin(), buffer.end(), RAW(out));
  return out;
}