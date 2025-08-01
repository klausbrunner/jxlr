#include <Rcpp.h>
#include <jxl/decode.h>
#include <jxl/encode.h>
#include <jxl/encode_cxx.h>
#include <jxl/thread_parallel_runner.h>
#include <jxl/thread_parallel_runner_cxx.h>
#include <algorithm>
#include <array>
#include <fstream>
#include <memory>
#include <vector>

using namespace Rcpp;

struct JxlDecoderDeleter {
    void operator()(JxlDecoder* decoder) noexcept {
        if (decoder) JxlDecoderDestroy(decoder);
    }
};

using JxlDecoderPtr = std::unique_ptr<JxlDecoder, JxlDecoderDeleter>;

namespace {
    void check_jxl_result(JxlDecoderStatus status, const char* context) {
        if (status != JXL_DEC_SUCCESS) {
            stop(std::string(context) + ": " + std::to_string(static_cast<int>(status)));
        }
    }

    void check_jxl_result(JxlEncoderStatus status, const char* context) {
        if (status != JXL_ENC_SUCCESS) {
            stop(std::string(context) + ": " + std::to_string(static_cast<int>(status)));
        }
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
}

// [[Rcpp::export]]
List jxl_info(const std::string& filename) {
    const auto data = read_file(filename);
    
    auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
    if (!decoder) stop("Failed to create JXL decoder");
    
    check_jxl_result(
        JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO),
        "Failed to subscribe to decoder events"
    );
    
    check_jxl_result(
        JxlDecoderSetInput(decoder.get(), data.data(), data.size()),
        "Failed to set decoder input"
    );
    
    JxlDecoderCloseInput(decoder.get());
    
    const auto status = JxlDecoderProcessInput(decoder.get());
    if (status != JXL_DEC_BASIC_INFO) {
        stop("Failed to decode JXL basic info");
    }
    
    JxlBasicInfo basic_info;
    check_jxl_result(
        JxlDecoderGetBasicInfo(decoder.get(), &basic_info),
        "Failed to get basic info"
    );
    
    return List::create(
        _["width"] = static_cast<int>(basic_info.xsize),
        _["height"] = static_cast<int>(basic_info.ysize),
        _["channels"] = static_cast<int>(basic_info.num_color_channels),
        _["alpha"] = basic_info.alpha_bits > 0,
        _["bits_per_sample"] = static_cast<int>(basic_info.bits_per_sample),
        _["uses_original_profile"] = basic_info.uses_original_profile
    );
}

// [[Rcpp::export("R_jxl_get_info")]]
IntegerVector jxl_get_info_raw(const RawVector& buf) {
    auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
    if (!decoder) stop("Failed to create JXL decoder");
    
    check_jxl_result(
        JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO),
        "Failed to subscribe to decoder events"
    );
    
    check_jxl_result(
        JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
        "Failed to set decoder input"
    );
    
    JxlDecoderCloseInput(decoder.get());
    
    if (JxlDecoderProcessInput(decoder.get()) != JXL_DEC_BASIC_INFO) {
        stop("Failed to decode JXL basic info");
    }
    
    JxlBasicInfo basic_info;
    check_jxl_result(
        JxlDecoderGetBasicInfo(decoder.get(), &basic_info),
        "Failed to get basic info"
    );
    
    return IntegerVector::create(
        static_cast<int>(basic_info.xsize),
        static_cast<int>(basic_info.ysize)
    );
}

// [[Rcpp::export("R_jxl_decode")]]
RawVector jxl_decode_raw(const RawVector& buf) {
    auto decoder = JxlDecoderPtr(JxlDecoderCreate(nullptr));
    if (!decoder) stop("Failed to create JXL decoder");
    
    check_jxl_result(
        JxlDecoderSubscribeEvents(decoder.get(), JXL_DEC_BASIC_INFO | JXL_DEC_FULL_IMAGE),
        "Failed to subscribe to decoder events"
    );
    
    check_jxl_result(
        JxlDecoderSetInput(decoder.get(), RAW(buf), LENGTH(buf)),
        "Failed to set decoder input"
    );
    
    JxlDecoderCloseInput(decoder.get());
    
    JxlBasicInfo basic_info;
    const JxlPixelFormat format = {4, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};
    
    while (true) {
        const auto status = JxlDecoderProcessInput(decoder.get());
        
        switch (status) {
            case JXL_DEC_ERROR:
            case JXL_DEC_NEED_MORE_INPUT:
                stop("JXL decoder error");
                
            case JXL_DEC_BASIC_INFO:
                check_jxl_result(
                    JxlDecoderGetBasicInfo(decoder.get(), &basic_info),
                    "Failed to get basic info"
                );
                break;
                
            case JXL_DEC_NEED_IMAGE_OUT_BUFFER: {
                size_t buffer_size;
                check_jxl_result(
                    JxlDecoderImageOutBufferSize(decoder.get(), &format, &buffer_size),
                    "Failed to get output buffer size"
                );
                
                RawVector image(buffer_size);
                check_jxl_result(
                    JxlDecoderSetImageOutBuffer(decoder.get(), &format, RAW(image), buffer_size),
                    "Failed to set image output buffer"
                );
                
                image.attr("dim") = IntegerVector::create(
                    4,
                    static_cast<int>(basic_info.xsize),
                    static_cast<int>(basic_info.ysize)
                );
                
                return image;
            }
            
            case JXL_DEC_FULL_IMAGE:
                continue;
                
            case JXL_DEC_SUCCESS:
                stop("Failed to decode image");
                
            default:
                stop("Unknown decoder status: " + std::to_string(static_cast<int>(status)));
        }
    }
}

// [[Rcpp::export("R_jxl_encode")]]
RawVector jxl_encode_raw(const RawVector& img, const IntegerVector& quality, const IntegerVector& effort) {
    const auto dim = as<IntegerVector>(img.attr("dim"));
    const auto channels = static_cast<uint32_t>(dim[0]);
    const auto width = static_cast<uint32_t>(dim[1]);
    const auto height = static_cast<uint32_t>(dim[2]);
    const auto qual = quality[0];
    const auto eff = effort[0];
    
    auto enc = JxlEncoderMake(nullptr);
    if (!enc) stop("Failed to create JXL encoder");
    
    auto runner = JxlThreadParallelRunnerMake(
        nullptr, 
        JxlThreadParallelRunnerDefaultNumWorkerThreads()
    );
    check_jxl_result(
        JxlEncoderSetParallelRunner(enc.get(), JxlThreadParallelRunner, runner.get()),
        "Failed to set parallel runner"
    );
    
    JxlBasicInfo basic_info;
    JxlEncoderInitBasicInfo(&basic_info);
    basic_info.xsize = width;
    basic_info.ysize = height;
    basic_info.bits_per_sample = 8;
    basic_info.exponent_bits_per_sample = 0;
    basic_info.num_color_channels = 3;
    basic_info.alpha_bits = (channels == 4) ? 8 : 0;
    basic_info.alpha_exponent_bits = 0;
    basic_info.num_extra_channels = (channels == 4) ? 1 : 0;
    basic_info.uses_original_profile = JXL_FALSE;
    
    check_jxl_result(
        JxlEncoderSetBasicInfo(enc.get(), &basic_info),
        "Failed to set basic info"
    );
    
    JxlColorEncoding color_encoding = {};
    JxlColorEncodingSetToSRGB(&color_encoding, JXL_FALSE);
    check_jxl_result(
        JxlEncoderSetColorEncoding(enc.get(), &color_encoding),
        "Failed to set color encoding"
    );
    
    auto* frame_settings = JxlEncoderFrameSettingsCreate(enc.get(), nullptr);
    if (!frame_settings) stop("Failed to create frame settings");
    
    const int effort_level = (eff == NA_INTEGER) ? 7 : eff;
    check_jxl_result(
        JxlEncoderFrameSettingsSetOption(frame_settings, JXL_ENC_FRAME_SETTING_EFFORT, effort_level),
        "Failed to set effort level"
    );
    
    if (qual == NA_INTEGER) {
        check_jxl_result(
            JxlEncoderSetFrameDistance(frame_settings, 0.0f),
            "Failed to set lossless distance"
        );
    } else {
        const float distance = std::max(0.1f, std::min(15.0f, (100.0f - qual) / 10.0f));
        check_jxl_result(
            JxlEncoderSetFrameDistance(frame_settings, distance),
            "Failed to set frame distance"
        );
    }
    
    const JxlPixelFormat pixel_format = {channels, JXL_TYPE_UINT8, JXL_NATIVE_ENDIAN, 0};
    
    check_jxl_result(
        JxlEncoderAddImageFrame(frame_settings, &pixel_format, RAW(img), LENGTH(img)),
        "Failed to add image frame"
    );
    
    JxlEncoderCloseInput(enc.get());
    
    std::vector<uint8_t> compressed(1024);
    auto* next_out = compressed.data();
    auto avail_out = compressed.size();
    
    while (true) {
        const auto result = JxlEncoderProcessOutput(enc.get(), &next_out, &avail_out);
        
        if (result == JXL_ENC_SUCCESS) {
            compressed.resize(compressed.size() - avail_out);
            break;
        }
        
        if (result == JXL_ENC_NEED_MORE_OUTPUT) {
            const auto offset = next_out - compressed.data();
            compressed.resize(compressed.size() * 2);
            next_out = compressed.data() + offset;
            avail_out = compressed.size() - offset;
        } else {
            stop("Failed to encode JXL image");
        }
    }
    
    RawVector out(compressed.size());
    std::copy(compressed.begin(), compressed.end(), RAW(out));
    return out;
}
