test_that("read_jxl_as format conversion works", {
  # Create test image in CHW format (jxlr native)
  img_chw <- array(runif(3 * 5 * 4), dim = c(3, 5, 4))
  jxl_data <- write_jxl(img_chw, quality = 90)

  # Read as CHW (default)
  img_read_chw <- read_jxl_as(jxl_data, format = "chw")
  expect_equal(dim(img_read_chw), c(3, 5, 4)) # Same as input

  # Read as HWC
  img_read_hwc <- read_jxl_as(jxl_data, format = "hwc")
  expect_equal(dim(img_read_hwc), c(5, 3, 4)) # H x W x C (RGBA output)

  # Read with auto format (should default to HWC)
  img_read_auto <- read_jxl_as(jxl_data, format = "auto")
  expect_equal(dim(img_read_auto), c(5, 3, 4)) # H x W x C (RGBA output)
})

test_that("write_jxl_as format conversion works", {
  # Create test image in HWC format (common in R)
  img_hwc <- array(runif(6 * 4 * 3), dim = c(6, 4, 3)) # H x W x C

  # Write from HWC format
  jxl_data <- write_jxl_as(img_hwc, format = "hwc", quality = 85)
  expect_s3_class(jxl_data, "jxl")
  expect_type(jxl_data, "raw")

  # Should be readable
  img_read <- read_jxl(jxl_data)
  expect_equal(dim(img_read), c(4, 6, 4)) # HWC input -> WHC output with RGBA

  # Write with auto-detection (should detect HWC from dimensions)
  jxl_data_auto <- write_jxl_as(img_hwc, format = "auto", quality = 85)
  expect_s3_class(jxl_data_auto, "jxl")
})

