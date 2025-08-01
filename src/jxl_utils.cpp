#include <Rcpp.h>
#include <jxl/decode.h>
#include <jxl/encode.h>
#include <jxl/encode_cxx.h>
#include <jxl/thread_parallel_runner.h>
#include <jxl/thread_parallel_runner_cxx.h>

#include <algorithm>
#include <array>
#include <fstream>
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

struct JxlDecoderDeleter {
  void operator()(JxlDecoder* decoder) noexcept {
    if (decoder) JxlDecoderDestroy(decoder);
  }
};

struct JxlEncoderDeleter {
  void operator()(JxlEncoder* encoder) noexcept {
    if (encoder) JxlEncoderDestroy(encoder);
  }
};

struct JxlThreadParallelRunnerDeleter {
  void operator()(void* runner) noexcept {
    if (runner) JxlThreadParallelRunnerDestroy(runner);
  }
};

using JxlDecoderPtr = std::unique_ptr<JxlDecoder, JxlDecoderDeleter>;

namespace {
// Constants
constexpr size_t DEFAULT_BUFFER_SIZE = 1024;
constexpr size_t ANIMATION_FRAMES_RESERVE =
    16;  // Typical animation frame count
constexpr int DEFAULT_EFFORT_LEVEL = 7;
constexpr int BITS_PER_SAMPLE = 8;
constexpr int ALPHA_BITS = 8;
constexpr int COLOR_CHANNELS = 3;
constexpr float MIN_DISTANCE = 0.1f;
constexpr float MAX_DISTANCE = 15.0f;
constexpr float QUALITY_SCALE = 10.0f;
constexpr float MAX_QUALITY = 100.0f;
constexpr uint32_t ANIMATION_TPS_NUMERATOR = 1000;
constexpr uint32_t ANIMATION_TPS_DENOMINATOR = 1;
constexpr int DEFAULT_FRAME_DURATION = 100;

struct JxlConfig {
  int quality = NA_INTEGER;
  int effort = DEFAULT_EFFORT_LEVEL;
  bool is_animation = false;
  uint32_t loop_count = 0;

  float distance() const noexcept {
    return (quality == NA_INTEGER)
               ? 0.0f
               : std::max(MIN_DISTANCE,
                          std::min(MAX_DISTANCE,
                                   (MAX_QUALITY - quality) / QUALITY_SCALE));
  }

  bool is_lossless() const noexcept { return quality == NA_INTEGER; }

  int effort_level() const noexcept {
    return (effort == NA_INTEGER) ? DEFAULT_EFFORT_LEVEL : effort;
  }
};

JxlBasicInfo make_basic_info(uint32_t width, uint32_t height, uint32_t channels,
                             bool is_animation = false) {
  JxlBasicInfo basic_info;
  JxlEncoderInitBasicInfo(&basic_info);
  basic_info.xsize = width;
  basic_info.ysize = height;
  basic_info.bits_per_sample = BITS_PER_SAMPLE;
  basic_info.exponent_bits_per_sample = 0;
  basic_info.num_color_channels = COLOR_CHANNELS;
  basic_info.alpha_bits = (channels == 4) ? ALPHA_BITS : 0;
  basic_info.alpha_exponent_bits = 0;
  basic_info.num_extra_channels = (channels == 4) ? 1 : 0;
  basic_info.uses_original_profile = JXL_FALSE;

  if (is_animation) {
    basic_info.have_animation = JXL_TRUE;
    basic_info.animation.tps_numerator = ANIMATION_TPS_NUMERATOR;
    basic_info.animation.tps_denominator = ANIMATION_TPS_DENOMINATOR;
    basic_info.animation.have_timecodes = JXL_FALSE;
  }

  return basic_info;
}

const std::unordered_map<int, std::string> jxl_status_messages = {
    // Success (shared between encoder/decoder)
    {JXL_DEC_SUCCESS, "Success"},
    {JXL_ENC_SUCCESS, "Success"},
    // Errors (shared between encoder/decoder)
    {JXL_DEC_ERROR, "Generic error"},
    {JXL_ENC_ERROR, "Generic error"},
    // Decoder-specific statuses
    {JXL_DEC_NEED_MORE_INPUT, "Need more input data"},
    {JXL_DEC_NEED_PREVIEW_OUT_BUFFER, "Need preview output buffer"},
    {JXL_DEC_NEED_IMAGE_OUT_BUFFER, "Need image output buffer"},
    {JXL_DEC_PREVIEW_IMAGE, "Preview image ready"},
    {JXL_DEC_FULL_IMAGE, "Full image ready"},
    {JXL_DEC_JPEG_NEED_MORE_OUTPUT, "JPEG reconstruction needs more output"},
    {JXL_DEC_BOX, "Box data ready"},
    {JXL_DEC_FRAME, "Frame data ready"},
    {JXL_DEC_COLOR_ENCODING, "Color encoding ready"},
    {JXL_DEC_JPEG_RECONSTRUCTION, "JPEG reconstruction ready"},
    {JXL_DEC_BOX_NEED_MORE_OUTPUT, "Box needs more output"},
    {JXL_DEC_BASIC_INFO, "Basic info ready"},
    // Encoder-specific statuses
    {JXL_ENC_NEED_MORE_OUTPUT, "Need more output buffer space"}};

template <typename StatusType>
std::string jxl_status_message(StatusType status) {
  const auto status_int = static_cast<int>(status);
  const auto it = jxl_status_messages.find(status_int);
  if (it != jxl_status_messages.end()) {
    return it->second;
  }
  std::string result = "Unknown JXL status (";
  result += std::to_string(status_int);
  result += ")";
  return result;
}

void check_jxl_result(JxlDecoderStatus status, const char* context) {
  if (status != JXL_DEC_SUCCESS) {
    stop(std::string(context) + ": " + jxl_status_message(status));
  }
}

void check_jxl_result(JxlEncoderStatus status, const char* context) {
  if (status != JXL_ENC_SUCCESS) {
    stop(std::string(context) + ": " + jxl_status_message(status));
  }
}

class JxlEncoderWrapper {
  JxlEncoderPtr encoder_;
  JxlThreadParallelRunnerPtr runner_;

 public:
  JxlEncoderWrapper()
      : encoder_(JxlEncoderMake(nullptr)),
        runner_(JxlThreadParallelRunnerMake(
            nullptr, JxlThreadParallelRunnerDefaultNumWorkerThreads())) {
    if (!encoder_) throw std::runtime_error("Failed to create JXL encoder");
    if (!runner_) throw std::runtime_error("Failed to create thread runner");

    check_jxl_result(
        JxlEncoderSetParallelRunner(encoder_.get(), JxlThreadParallelRunner,
                                    runner_.get()),
        "Failed to set parallel runner");
  }

  JxlEncoder* get() { return encoder_.get(); }
  const JxlEncoder* get() const { return encoder_.get(); }

  void setup_basic_info(const JxlBasicInfo& info) {
    check_jxl_result(JxlEncoderSetBasicInfo(encoder_.get(), &info),
                     "Failed to set basic info");

    JxlColorEncoding color_encoding = {};
    JxlColorEncodingSetToSRGB(&color_encoding, JXL_FALSE);
    check_jxl_result(
        JxlEncoderSetColorEncoding(encoder_.get(), &color_encoding),
        "Failed to set color encoding");
  }

  JxlEncoderFrameSettings* create_frame_settings() {
    auto* settings = JxlEncoderFrameSettingsCreate(encoder_.get(), nullptr);
    if (!settings) throw std::runtime_error("Failed to create frame settings");
    return settings;
  }

  void close_input() { JxlEncoderCloseInput(encoder_.get()); }
};

class JxlDecoderWrapper {
  JxlDecoderPtr decoder_;

 public:
  explicit JxlDecoderWrapper(int events = JXL_DEC_BASIC_INFO |
                                          JXL_DEC_FULL_IMAGE)
      : decoder_(JxlDecoderCreate(nullptr)) {
    if (!decoder_) throw std::runtime_error("Failed to create JXL decoder");
    check_jxl_result(JxlDecoderSubscribeEvents(decoder_.get(), events),
                     "Failed to subscribe to decoder events");
  }

  JxlDecoder* get() { return decoder_.get(); }
  const JxlDecoder* get() const { return decoder_.get(); }

  void set_input(const uint8_t* data, size_t size) {
    check_jxl_result(JxlDecoderSetInput(decoder_.get(), data, size),
                     "Failed to set decoder input");
    JxlDecoderCloseInput(decoder_.get());
  }

  JxlDecoderStatus process() { return JxlDecoderProcessInput(decoder_.get()); }

  JxlBasicInfo get_basic_info() {
    JxlBasicInfo info;
    check_jxl_result(JxlDecoderGetBasicInfo(decoder_.get(), &info),
                     "Failed to get basic info");
    return info;
  }
};

class DynamicBuffer {
  std::vector<uint8_t> buffer_;
  uint8_t* next_out_;
  size_t avail_out_;

 public:
  explicit DynamicBuffer(size_t initial = DEFAULT_BUFFER_SIZE)
      : buffer_(initial) {
    next_out_ = buffer_.data();
    avail_out_ = buffer_.size();
  }

  bool ensure_space() {
    if (avail_out_ == 0) {
      const auto offset = next_out_ - buffer_.data();
      buffer_.resize(buffer_.size() * 2);
      next_out_ = buffer_.data() + offset;
      avail_out_ = buffer_.size() - offset;
      return true;
    }
    return false;
  }

  void finalize() { buffer_.resize(buffer_.size() - avail_out_); }

  uint8_t** next_out() { return &next_out_; }
  size_t* avail_out() { return &avail_out_; }
  const std::vector<uint8_t>& data() const { return buffer_; }
};

void configure_frame_settings(JxlEncoderFrameSettings* frame_settings,
                              const JxlConfig& config) {
  check_jxl_result(
      JxlEncoderFrameSettingsSetOption(
          frame_settings, JXL_ENC_FRAME_SETTING_EFFORT, config.effort_level()),
      "Failed to set effort level");

  if (config.is_lossless()) {
    check_jxl_result(JxlEncoderSetFrameDistance(frame_settings, 0.0f),
                     "Failed to set lossless distance");
  } else {
    check_jxl_result(
        JxlEncoderSetFrameDistance(frame_settings, config.distance()),
        "Failed to set frame distance");
  }
}

RawVector encode_with_dynamic_buffer(
    const std::function<void(JxlEncoderWrapper&)>& setup_encoder) {
  JxlEncoderWrapper encoder;
  setup_encoder(encoder);
  encoder.close_input();

  DynamicBuffer buffer;
  while (true) {
    const auto result = JxlEncoderProcessOutput(
        encoder.get(), buffer.next_out(), buffer.avail_out());

    if (result == JXL_ENC_SUCCESS) {
      buffer.finalize();
      break;
    }

    if (result == JXL_ENC_NEED_MORE_OUTPUT) {
      buffer.ensure_space();
    } else {
      stop("Failed to encode JXL: " + jxl_status_message(result));
    }
  }

  const auto& compressed = buffer.data();
  RawVector out(compressed.size());
  std::copy(compressed.begin(), compressed.end(), RAW(out));
  return out;
}

std::vector<uint8_t> read_file(const std::string& filename) {
  std::ifstream file(filename, std::ios::binary | std::ios::ate);
  if (!file.is_open()) {
    stop("Cannot open file: " + filename);
  }

  const auto size = file.tellg();
  file.seekg(0, std::ios::beg);

  std::vector<uint8_t> data(static_cast<size_t>(size));
  file.read(reinterpret_cast<char*>(data.data()), size);
  return data;
}
}  // namespace

// [[Rcpp::export]]
List jxl_info(const std::string& filename) {
  const auto data = read_file(filename);
  JxlDecoderWrapper decoder(JXL_DEC_BASIC_INFO);
  decoder.set_input(data.data(), data.size());

  const auto status = decoder.process();
  if (status != JXL_DEC_BASIC_INFO) {
    stop("Failed to decode JXL basic info: " + jxl_status_message(status));
  }

  const auto basic_info = decoder.get_basic_info();
  return List::create(
      _["width"] = static_cast<int>(basic_info.xsize),
      _["height"] = static_cast<int>(basic_info.ysize),
      _["channels"] = static_cast<int>(basic_info.num_color_channels),
      _["alpha"] = basic_info.alpha_bits > 0,
      _["bits_per_sample"] = static_cast<int>(basic_info.bits_per_sample),
      _["uses_original_profile"] = basic_info.uses_original_profile);
}

// [[Rcpp::export("R_jxl_get_info")]]
IntegerVector jxl_get_info_raw(const RawVector& buf) {
  JxlDecoderWrapper decoder(JXL_DEC_BASIC_INFO);
  decoder.set_input(RAW(buf), LENGTH(buf));

  const auto status = decoder.process();
  if (status != JXL_DEC_BASIC_INFO) {
    stop("Failed to decode JXL basic info: " + jxl_status_message(status));
  }

  const auto basic_info = decoder.get_basic_info();
  return IntegerVector::create(static_cast<int>(basic_info.xsize),
                               static_cast<int>(basic_info.ysize));
}

// [[Rcpp::export("R_jxl_decode")]]
RawVector jxl_decode_raw(const RawVector& buf) {
  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create JXL decoder");

  check_jxl_result(JxlDecoderSubscribeEvents(
                       decoder.get(), JXL_DEC_BASIC_INFO | JXL_DEC_FULL_IMAGE),
                   "Failed to subscribe to decoder events");

  check_jxl_result(JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
                   "Failed to set decoder input");

  JxlDecoderCloseInput(decoder.get());

  JxlBasicInfo basic_info;
  const JxlPixelFormat format = {4, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};

  while (true) {
    const auto status = JxlDecoderProcessInput(decoder.get());

    switch (status) {
      case JXL_DEC_ERROR:
        stop("JXL decoder error: " + jxl_status_message(status));
      case JXL_DEC_NEED_MORE_INPUT:
        stop("JXL decoder error: " + jxl_status_message(status));

      case JXL_DEC_BASIC_INFO:
        check_jxl_result(JxlDecoderGetBasicInfo(decoder.get(), &basic_info),
                         "Failed to get basic info");
        break;

      case JXL_DEC_NEED_IMAGE_OUT_BUFFER: {
        size_t buffer_size;
        check_jxl_result(
            JxlDecoderImageOutBufferSize(decoder.get(), &format, &buffer_size),
            "Failed to get output buffer size");

        RawVector image(buffer_size);
        check_jxl_result(JxlDecoderSetImageOutBuffer(decoder.get(), &format,
                                                     RAW(image), buffer_size),
                         "Failed to set image output buffer");

        image.attr("dim") =
            IntegerVector::create(4, static_cast<int>(basic_info.xsize),
                                  static_cast<int>(basic_info.ysize));

        return image;
      }

      case JXL_DEC_FULL_IMAGE:
        continue;

      case JXL_DEC_SUCCESS:
        stop("Unexpected success status - failed to decode image");

      default:
        stop("Unknown decoder status: " + jxl_status_message(status));
    }
  }
}

// [[Rcpp::export("R_jxl_encode")]]
RawVector jxl_encode_raw(const RawVector& img, const IntegerVector& quality,
                         const IntegerVector& effort) {
  const auto dim = as<IntegerVector>(img.attr("dim"));
  const auto channels = static_cast<uint32_t>(dim[0]);
  const auto width = static_cast<uint32_t>(dim[1]);
  const auto height = static_cast<uint32_t>(dim[2]);

  JxlConfig config{quality[0], effort[0]};

  return encode_with_dynamic_buffer([&](JxlEncoderWrapper& encoder) {
    const auto basic_info = make_basic_info(width, height, channels);
    encoder.setup_basic_info(basic_info);

    auto* frame_settings = encoder.create_frame_settings();
    configure_frame_settings(frame_settings, config);

    const JxlPixelFormat pixel_format = {channels, JXL_TYPE_UINT8,
                                         JXL_NATIVE_ENDIAN, 0};
    check_jxl_result(JxlEncoderAddImageFrame(frame_settings, &pixel_format,
                                             RAW(img), LENGTH(img)),
                     "Failed to add image frame");
  });
}

// [[Rcpp::export("R_jxl_decode_anim")]]
List jxl_decode_anim_raw(const RawVector& buf) {
  auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
  if (!decoder) stop("Failed to create JXL decoder");

  check_jxl_result(JxlDecoderSubscribeEvents(
                       decoder.get(),
                       JXL_DEC_BASIC_INFO | JXL_DEC_FRAME | JXL_DEC_FULL_IMAGE),
                   "Failed to subscribe to decoder events");
  check_jxl_result(JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
                   "Failed to set decoder input");
  JxlDecoderCloseInput(decoder.get());

  JxlBasicInfo basic_info;
  const JxlPixelFormat format = {4, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};
  std::vector<RawVector> frames;
  std::vector<int> durations;
  frames.reserve(ANIMATION_FRAMES_RESERVE);
  durations.reserve(ANIMATION_FRAMES_RESERVE);

  bool decoding_complete = false;
  while (!decoding_complete) {
    const auto status = JxlDecoderProcessInput(decoder.get());

    switch (status) {
      case JXL_DEC_ERROR:
        stop("JXL decoder error: " + jxl_status_message(status));
      case JXL_DEC_NEED_MORE_INPUT:
        stop("JXL decoder error: " + jxl_status_message(status));

      case JXL_DEC_BASIC_INFO:
        check_jxl_result(JxlDecoderGetBasicInfo(decoder.get(), &basic_info),
                         "Failed to get basic info");
        break;

      case JXL_DEC_FRAME: {
        JxlFrameHeader frame_header;
        check_jxl_result(JxlDecoderGetFrameHeader(decoder.get(), &frame_header),
                         "Failed to get frame header");
        durations.push_back(static_cast<int>(
            frame_header.duration * basic_info.animation.tps_denominator *
            1000.0f / basic_info.animation.tps_numerator));
        break;
      }

      case JXL_DEC_NEED_IMAGE_OUT_BUFFER: {
        size_t buffer_size;
        check_jxl_result(
            JxlDecoderImageOutBufferSize(decoder.get(), &format, &buffer_size),
            "Failed to get output buffer size");
        RawVector image(buffer_size);
        check_jxl_result(JxlDecoderSetImageOutBuffer(decoder.get(), &format,
                                                     RAW(image), buffer_size),
                         "Failed to set image output buffer");
        image.attr("dim") =
            IntegerVector::create(4, static_cast<int>(basic_info.xsize),
                                  static_cast<int>(basic_info.ysize));
        frames.push_back(image);
        break;
      }

      case JXL_DEC_FULL_IMAGE:
        continue;

      case JXL_DEC_SUCCESS:
        decoding_complete = true;
        break;

      default:
        stop("Unknown decoder status: " + jxl_status_message(status));
    }
  }

  return List::create(
      _["frames"] = frames, _["durations"] = durations,
      _["width"] = static_cast<int>(basic_info.xsize),
      _["height"] = static_cast<int>(basic_info.ysize),
      _["loop_count"] = static_cast<int>(basic_info.animation.num_loops));
}

JxlFrameHeader make_frame_header(int duration = DEFAULT_FRAME_DURATION) {
  JxlFrameHeader frame_header;
  JxlEncoderInitFrameHeader(&frame_header);
  frame_header.duration = duration;
  frame_header.layer_info.have_crop = JXL_FALSE;
  frame_header.layer_info.blend_info.blendmode = JXL_BLEND_REPLACE;
  frame_header.layer_info.blend_info.source = 0;
  frame_header.layer_info.blend_info.alpha = 0;
  frame_header.layer_info.blend_info.clamp = JXL_FALSE;
  frame_header.layer_info.save_as_reference = 0;
  return frame_header;
}

// [[Rcpp::export("R_jxl_encode_anim")]]
RawVector jxl_encode_anim_raw(const List& frames,
                              const IntegerVector& durations,
                              const IntegerVector& quality,
                              const IntegerVector& effort,
                              const IntegerVector& loop_count) {
  if (frames.size() == 0) stop("No frames provided");

  const auto first_frame = as<RawVector>(frames[0]);
  const auto dim = as<IntegerVector>(first_frame.attr("dim"));
  const auto channels = static_cast<uint32_t>(dim[0]);
  const auto width = static_cast<uint32_t>(dim[1]);
  const auto height = static_cast<uint32_t>(dim[2]);

  JxlConfig config{quality[0], effort[0], true,
                   static_cast<uint32_t>(loop_count[0])};

  return encode_with_dynamic_buffer([&](JxlEncoderWrapper& encoder) {
    auto basic_info = make_basic_info(width, height, channels, true);
    basic_info.animation.num_loops =
        (loop_count[0] == NA_INTEGER) ? 0 : config.loop_count;
    encoder.setup_basic_info(basic_info);

    const JxlPixelFormat pixel_format = {channels, JXL_TYPE_UINT8,
                                         JXL_NATIVE_ENDIAN, 0};

    for (size_t i = 0; i < frames.size(); ++i) {
      auto* frame_settings = encoder.create_frame_settings();
      configure_frame_settings(frame_settings, config);

      const int duration = (i < static_cast<size_t>(durations.size()))
                               ? durations[static_cast<int>(i)]
                               : DEFAULT_FRAME_DURATION;
      const auto frame_header = make_frame_header(duration);
      check_jxl_result(JxlEncoderSetFrameHeader(frame_settings, &frame_header),
                       "Failed to set frame header");

      const auto frame = as<RawVector>(frames[i]);
      check_jxl_result(JxlEncoderAddImageFrame(frame_settings, &pixel_format,
                                               RAW(frame), LENGTH(frame)),
                       "Failed to add image frame");
    }
  });
}
