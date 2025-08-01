#' Read JPEG XL images
#'
#' Read JPEG XL images into a bitmap array using the same conventions as the
#' \pkg{png} and \pkg{jpeg} packages.
#'
#' @param source A raw vector or path to a JXL file
#' @param numeric Logical; convert image to 0-1 real numbers for compatibility
#'   with \pkg{jpeg} and \pkg{png} packages
#' @return A 3D array representing the image. If \code{numeric = TRUE}, values
#'   are in \\[0,1\\], otherwise raw bytes. Always returns RGBA format.
#'
#' @examples
#' \donttest{
#' img <- array(runif(10 * 10 * 3), dim = c(10, 10, 3))
#'
#' jxl_data <- write_jxl(img, quality = 80)
#'
#' img_back <- read_jxl(jxl_data)
#' cat("Original:", dim(img), "Decoded:", dim(img_back), "\n")
#' }
#'
#' \dontrun{
#' img <- read_jxl("image.jxl")
#' write_jxl(img, "output.jxl", quality = 90)
#' }
#' @export
read_jxl <- function(source, numeric = TRUE) {
  if (is.character(source)) {
    if (!file.exists(source[1])) {
      stop("File does not exist: ", source[1])
    }
    source <- readBin(source[1], raw(), file.info(source)$size)
  }
  stopifnot("source must be raw vector or file path" = is.raw(source))

  out <- .Call("_jxlr_jxl_decode_raw", source, PACKAGE = "jxlr")

  if (isTRUE(numeric)) {
    out <- structure(as.numeric(out) / 255, dim = dim(out))
    out <- aperm(out)
  } else {
    class(out) <- c("rawimg", class(out))
  }

  out
}

#' Write JPEG XL images
#'
#' Write image arrays to JPEG XL format with configurable quality settings.
#'
#' @param image 3D array (width x height x channels) with values in \\[0,1\\].
#'   Must have 3 (RGB) or 4 (RGBA) channels.
#' @param target Output file path, or \code{NULL} to return raw vector
#' @param quality Compression quality: 0-100 for lossy, \code{NA} for lossless
#' @param effort Compression effort: 1-9 (1=fastest, 9=best compression, default=7)
#' @return If \code{target} is \code{NULL}, returns raw vector with class "jxl".
#'   Otherwise, invisibly returns the file path.
#'
#' @examples
#' \donttest{
#' img <- array(runif(8 * 8 * 3), dim = c(8, 8, 3))
#'
#' jxl_fast <- write_jxl(img, quality = 85, effort = 3)
#' jxl_best <- write_jxl(img, quality = 85, effort = 9)
#' cat("Fast:", length(jxl_fast), "bytes, Best:", length(jxl_best), "bytes\n")
#'
#' jxl_lossless <- write_jxl(img, quality = NA, effort = 8)
#' cat("Lossless:", length(jxl_lossless), "bytes\n")
#' }
#'
#' \dontrun{
#' write_jxl(img, "output.jxl", quality = 90)
#' }
#' @export
write_jxl <- function(image, target = NULL, quality = 90, effort = 7) {
  image <- prepare_image_for_encoding(image)
  channels <- dim(image)[1]
  quality <- as.integer(quality)
  effort <- as.integer(effort)

  validate_encoding_params(quality, effort)
  stopifnot("image must have 3 or 4 channels" = channels %in% c(3, 4))

  buf <- .Call("_jxlr_jxl_encode_raw", image, quality, effort, PACKAGE = "jxlr")

  if (is.character(target)) {
    writeBin(buf, target)
    invisible(target)
  } else {
    structure(buf, class = "jxl")
  }
}

validate_encoding_params <- function(quality, effort) {
  if (!is.na(quality)) {
    stopifnot("quality must be numeric" = is.numeric(quality))
    stopifnot("quality must be between 0 and 100" = quality >= 0 && quality <= 100)
  }
  if (!is.na(effort)) {
    stopifnot("effort must be numeric" = is.numeric(effort))
    stopifnot("effort must be an integer between 1 and 9" = effort == round(effort) && effort >= 1 && effort <= 9)
  }
}

prepare_image_for_encoding <- function(image) {
  if (is.numeric(image)) {
    image <- structure(as.raw(image * 255), dim = dim(image))
    image <- aperm(image)
  }
  image
}

normalize_frame <- function(frame) {
  frame <- structure(as.numeric(frame) / 255, dim = dim(frame))
  aperm(frame)
}

jxl_dims <- function(buf) {
  stopifnot("buf must be raw vector" = is.raw(buf))
  .Call("_jxlr_jxl_get_info_raw", buf, PACKAGE = "jxlr")
}

#' @export
print.rawimg <- function(x, ...) {
  dims <- dim(x)
  cat(sprintf(
    "Raw image: %d x %d pixels, %d channels\n",
    dims[2], dims[3], dims[1]
  ))
  invisible(x)
}

#' @export
print.jxl <- function(x, ...) {
  dims <- jxl_dims(x)
  cat(sprintf(
    "JXL image buffer: %d x %d pixels (%d bytes)\n",
    dims[1], dims[2], length(x)
  ))
  invisible(x)
}

#' Read JPEG XL animations
#'
#' Read multi-frame JPEG XL files as animation objects with frame metadata.
#'
#' @param source A raw vector or path to a JXL animation file
#' @param numeric Logical; convert frames to 0-1 real numbers for compatibility
#' @return A jxl_anim object containing frames, durations, and metadata
#'
#' @examples
#' \donttest{
#' frames <- list(
#'   array(runif(8 * 8 * 3), dim = c(8, 8, 3)),
#'   array(runif(8 * 8 * 3), dim = c(8, 8, 3))
#' )
#' anim_data <- write_jxl_anim(frames, durations = c(500, 300))
#' anim <- read_jxl_anim(anim_data)
#' length(anim) # Number of frames
#' }
#' @export
read_jxl_anim <- function(source, numeric = TRUE) {
  if (is.character(source)) {
    source <- readBin(source[1], raw(), file.info(source)$size)
  }
  stopifnot("source must be raw vector or file path" = is.raw(source))

  result <- .Call("_jxlr_jxl_decode_anim_raw", source, PACKAGE = "jxlr")

  if (isTRUE(numeric)) {
    result$frames <- lapply(result$frames, normalize_frame)
  } else {
    result$frames <- lapply(result$frames, function(frame) {
      structure(frame, class = c("rawimg", class(frame)))
    })
  }

  structure(result, class = "jxl_anim")
}

#' Write JPEG XL animations
#'
#' Write frame lists to JPEG XL animation format with timing and loop control.
#'
#' @param frames List of 3D arrays (width x height x channels) with values in \\[0,1\\]
#' @param target Output file path, or NULL to return raw vector
#' @param durations Frame duration(s) in milliseconds. Single value applies to all frames, vector must match frame count
#' @param quality Compression quality: 0-100 for lossy, NA for lossless
#' @param effort Compression effort: 1-9 (1=fastest, 9=best compression, default=7)
#' @param loop_count Number of loops: 0 for infinite, NA for single play
#' @return If target is NULL, returns raw vector with class "jxl_anim".
#'   Otherwise, invisibly returns the file path.
#'
#' @examples
#' \donttest{
#' frames <- list(
#'   array(runif(5 * 5 * 3), dim = c(5, 5, 3)),
#'   array(runif(5 * 5 * 3), dim = c(5, 5, 3)),
#'   array(runif(5 * 5 * 3), dim = c(5, 5, 3))
#' )
#'
#' # Different duration per frame
#' anim1 <- write_jxl_anim(frames, durations = c(200, 300, 250))
#'
#' # Same duration for all frames
#' anim2 <- write_jxl_anim(frames, durations = 200)
#' cat("Animation sizes:", length(anim1), length(anim2), "bytes\n")
#' }
#' @export
write_jxl_anim <- function(frames, target = NULL, durations = 100,
                           quality = 90, effort = 7, loop_count = 0) {
  if (length(frames) == 0) stop("No frames provided")

  processed_frames <- lapply(frames, function(frame) {
    frame <- prepare_image_for_encoding(frame)
    stopifnot("frame must have 3 or 4 channels" = dim(frame)[1] %in% c(3, 4))
    frame
  })

  if (length(durations) == 1) durations <- rep(durations, length(frames))
  stopifnot("durations length must match frames length" = length(durations) == length(frames))

  durations <- as.integer(durations)
  quality <- as.integer(quality)
  effort <- as.integer(effort)
  loop_count <- as.integer(loop_count)

  validate_encoding_params(quality, effort)

  buf <- .Call("_jxlr_jxl_encode_anim_raw", processed_frames, durations, quality, effort, loop_count, PACKAGE = "jxlr")

  if (is.character(target)) {
    writeBin(buf, target)
    invisible(target)
  } else {
    structure(buf, class = "jxl_anim")
  }
}

#' @export
length.jxl_anim <- function(x) length(x$frames)

#' @export
`[.jxl_anim` <- function(x, i) {
  result <- list(
    frames = x$frames[i],
    durations = x$durations[i],
    width = x$width,
    height = x$height,
    loop_count = x$loop_count
  )
  structure(result, class = "jxl_anim")
}

#' @export
`[[.jxl_anim` <- function(x, i) x$frames[[i]]

#' @export
print.jxl_anim <- function(x, ...) {
  cat(sprintf("JXL animation: %d frames, %d x %d pixels\n", length(x$frames), x$width, x$height))
  if (length(x$durations) > 0) {
    durations_str <- paste(head(x$durations, 6), collapse = ", ")
    if (length(x$durations) > 6) durations_str <- paste(durations_str, "...")
    cat(sprintf("Frame durations: %s ms\n", durations_str))
  }
  cat(sprintf("Loops: %s\n", if (!is.na(x$loop_count) && x$loop_count > 0) paste(x$loop_count, "times") else "infinite"))
  invisible(x)
}

#' Read JXL with automatic format conversion
#'
#' Convenience wrapper that reads JXL and converts to specified format.
#'
#' @param source A raw vector or path to a JXL file
#' @param format Output format: "chw" (channels first), "hwc" (channels last), or "auto" (same as input)
#' @param numeric Logical; convert to 0-1 real numbers
#' @return Image array in requested format
#'
#' @examples
#' \donttest{
#' img <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
#' jxl_data <- write_jxl(img)
#'
#' # Read as channels-first (default JXL format)
#' img_chw <- read_jxl_as(jxl_data, "chw")
#' dim(img_chw) # c(4, 5, 5) - note RGBA output
#'
#' # Read as channels-last (common R format)
#' img_hwc <- read_jxl_as(jxl_data, "hwc")
#' dim(img_hwc) # c(5, 5, 4)
#' }
#' @export
read_jxl_as <- function(source, format = c("chw", "hwc", "auto"), numeric = TRUE) {
  format <- match.arg(format)

  img <- read_jxl(source, numeric = numeric)

  if (format == "hwc") {
    img <- aperm(img, c(2, 1, 3))  # W x H x C -> H x W x C
  } else if (format == "auto") {
    # Try to detect input format - this is a heuristic
    # Most R users expect HWC format
    img <- aperm(img, c(2, 1, 3))  # W x H x C -> H x W x C
  }

  img
}

#' Write JXL with automatic format conversion
#'
#' Convenience wrapper that converts from specified format and writes JXL.
#'
#' @param image Image array
#' @param target Output file path, or NULL to return raw vector
#' @param format Input format: "chw" (channels first), "hwc" (channels last), or "auto" (detect)
#' @param ... Additional arguments passed to write_jxl
#' @return Same as write_jxl
#'
#' @examples
#' \donttest{
#' # Common R format: Height x Width x Channels
#' img_hwc <- array(runif(8 * 6 * 3), dim = c(8, 6, 3))
#'
#' # Write directly from HWC format (will convert to WHC internally)
#' jxl_data <- write_jxl_as(img_hwc, format = "hwc", quality = 90)
#' }
#' @export
write_jxl_as <- function(image, target = NULL, format = c("auto", "chw", "hwc"), ...) {
  format <- match.arg(format)

  if (format == "hwc" || (format == "auto" && length(dim(image)) == 3 && dim(image)[3] <= 4)) {
    # Convert from HWC to WHC (what write_jxl expects)
    image <- aperm(image, c(2, 1, 3))  # H x W x C -> W x H x C
  }

  write_jxl(image, target = target, ...)
}

#' Convert R plots to JXL format
#'
#' Save any R plot (ggplot2, base R, lattice, etc.) to JXL format.
#'
#' @param filename Output JXL file path
#' @param expr Expression that produces a plot (e.g., \code{print(ggplot_obj)} or \code{plot(x, y)})
#' @param width,height Plot dimensions in inches
#' @param dpi Resolution in dots per inch
#' @param ... Additional arguments passed to \code{write_jxl_as}
#'
#' @details
#' This is a convenience function that renders plots to JXL format. Due to R's
#' graphics architecture, it uses PNG as an intermediate format - while not ideal,
#' this provides reliable cross-platform support for all R plotting systems.
#'
#' @examples
#' \donttest{
#' # Base R plots
#' plot_to_jxl(tempfile(fileext = ".jxl"), plot(mtcars$mpg, mtcars$wt), width = 5, height = 4)
#'
#' # Any plotting expression
#' plot_to_jxl(tempfile(fileext = ".jxl"), {
#'   hist(rnorm(1000), col = "lightblue", main = "Random Normal")
#' })
#'
#' # Multiple plots
#' plot_to_jxl(tempfile(fileext = ".jxl"), {
#'   par(mfrow = c(1, 2))
#'   plot(1:10, main = "Linear")
#'   plot(1:10, (1:10)^2, main = "Quadratic")
#' })
#' }
#' @export
plot_to_jxl <- function(filename, expr, width = 6, height = 4, dpi = 300, ...) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("png package required for plot_to_jxl()")
  }

  temp_png <- tempfile(fileext = ".png")

  tryCatch({
    png(temp_png, width = width * dpi, height = height * dpi, res = dpi)
    tryCatch(
      {
        eval.parent(substitute(expr))
      },
      error = function(e) {
        dev.off() # Close device before re-throwing error
        stop(e)
      }
    )
    dev.off()

    img <- png::readPNG(temp_png)
    write_jxl_as(img, filename, format = "hwc", ...)
  }, finally = {
    unlink(temp_png)
  })

  invisible(filename)
}
