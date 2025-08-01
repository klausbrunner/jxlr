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
#' img <- array(runif(10*10*3), dim = c(10, 10, 3))
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
  if (is.numeric(image)) {
    image <- structure(as.raw(image * 255), dim = dim(image))
    image <- aperm(image)
  }
  
  channels <- dim(image)[1]
  quality <- as.integer(quality)
  effort <- as.integer(effort)
  
  if (!is.na(quality)) {
    stopifnot("quality must be between 0 and 100" = quality >= 0 && quality <= 100)
  }
  if (!is.na(effort)) {
    stopifnot("effort must be between 1 and 9" = effort >= 1 && effort <= 9)
  }
  stopifnot("image must have 3 or 4 channels" = channels %in% c(3, 4))
  
  buf <- .Call("_jxlr_jxl_encode_raw", image, quality, effort, PACKAGE = "jxlr")
  
  if (is.character(target)) {
    writeBin(buf, target)
    invisible(target)
  } else {
    structure(buf, class = "jxl")
  }
}

jxl_dims <- function(buf) {
  stopifnot("buf must be raw vector" = is.raw(buf))
  .Call("_jxlr_jxl_get_info_raw", buf, PACKAGE = "jxlr")
}

#' @export
print.rawimg <- function(x, ...) {
  dims <- dim(x)
  cat(sprintf("Raw image: %d x %d pixels, %d channels\n", 
              dims[2], dims[3], dims[1]))
  invisible(x)
}

#' @export
print.jxl <- function(x, ...) {
  dims <- jxl_dims(x)
  cat(sprintf("JXL image buffer: %d x %d pixels (%d bytes)\n", 
              dims[1], dims[2], length(x)))
  invisible(x)
}