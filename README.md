# jxlr

> **Early Development**: This package is still in an early/experimental stage. Stuff may break. API may change.

R package for reading and writing JPEG XL (JXL) images. Features:

- RGB/RGBA support
- lossy and lossless compression
- multi-frame animations with timing control
- file and raw vector I/O

## Installation

```r
# Install from GitHub (requires libjxl system library)
remotes::install_github("klausbrunner/jxlr")
```

## Usage

```r
library(jxlr)

# Read/write images
img <- read_jxl("image.jxl")
write_jxl(img, "output.jxl", quality = 90, effort = 7)

# Animations
frames <- list(img1, img2, img3)
write_jxl_anim(frames, "anim.jxl", durations = c(200, 300, 250))
anim <- read_jxl_anim("anim.jxl")

# Handle different dimension formats
write_jxl_as(img_hwc, format = "hwc")  # Height×Width×Channels input
img_hwc <- read_jxl_as("image.jxl", format = "hwc")  # Read as H×W×C

# Save plots directly
plot_to_jxl("plot.jxl", plot(mtcars$mpg, mtcars$wt))

# Get image info
info <- jxl_info("image.jxl")
```

## Requirements

- R ≥ 4.1.0
- C++17 compiler  
- libjxl ≥ 0.7.0

### macOS (Homebrew)
```bash
brew install jpeg-xl
```

### Ubuntu/Debian
```bash
sudo apt install libjxl-dev
```
