# jxlr

> **Early Development**: This package is still in an early/experimental stage. Stuff may break. API may change.

R package for reading and writing JPEG XL (JXL) images.

## Installation

```r
# Install from GitHub (requires libjxl system library)
remotes::install_github("klausbrunner/jxlr")
```

## Usage

```r
library(jxlr)

# Read JXL image
img <- read_jxl("image.jxl")

# Write with quality control
write_jxl(img, "output.jxl", quality = 90)  # lossy
write_jxl(img, "output.jxl", quality = NA)  # lossless

# Get image info
info <- jxl_info("image.jxl")
```

## Requirements

- R ≥ 4.0.0
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

## Features

- RGB/RGBA support
- lossy and lossless compression
- compatible with png/jpeg package formats
- file and raw vector I/O
