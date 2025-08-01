test_that("Format conversion functions work", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  
  # Test channels-last to channels-first conversion
  img_hwc <- array(runif(4 * 3 * 3), dim = c(4, 3, 3))  # H x W x C
  img_chw <- from_channels_last(img_hwc)
  expect_equal(dim(img_chw), c(3, 4, 3))  # C x H x W
  
  # Test channels-first to channels-last conversion  
  img_back <- to_channels_last(img_chw)
  expect_equal(dim(img_back), c(4, 3, 3))  # H x W x C
  
  # Values should be preserved (though rearranged)
  expect_equal(sum(img_hwc), sum(img_chw))
  expect_equal(sum(img_hwc), sum(img_back))
})

test_that("read_jxl_as format conversion works", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  
  # Create test image in CHW format (jxlr native)
  img_chw <- array(runif(3 * 5 * 4), dim = c(3, 5, 4))
  jxl_data <- write_jxl(img_chw, quality = 90)
  
  # Read as CHW (default)
  img_read_chw <- read_jxl_as(jxl_data, format = "chw")
  expect_equal(dim(img_read_chw), c(3, 5, 4))  # Same as input
  
  # Read as HWC
  img_read_hwc <- read_jxl_as(jxl_data, format = "hwc") 
  expect_equal(dim(img_read_hwc), c(5, 3, 4))  # H x W x C (RGBA output)
  
  # Read with auto format (should default to HWC)
  img_read_auto <- read_jxl_as(jxl_data, format = "auto")
  expect_equal(dim(img_read_auto), c(5, 3, 4))  # H x W x C (RGBA output)
})

test_that("write_jxl_as format conversion works", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  
  # Create test image in HWC format (common in R)
  img_hwc <- array(runif(6 * 4 * 3), dim = c(6, 4, 3))  # H x W x C
  
  # Write from HWC format
  jxl_data <- write_jxl_as(img_hwc, format = "hwc", quality = 85)
  expect_s3_class(jxl_data, "jxl")
  expect_type(jxl_data, "raw")
  
  # Should be readable
  img_read <- read_jxl(jxl_data)
  expect_equal(dim(img_read), c(4, 6, 4))  # HWC input -> WHC output with RGBA
  
  # Write with auto-detection (should detect HWC from dimensions)
  jxl_data_auto <- write_jxl_as(img_hwc, format = "auto", quality = 85)
  expect_s3_class(jxl_data_auto, "jxl")
})

test_that("Integration helper input validation works", {
  # Test dimension validation
  expect_error(from_channels_last(array(1, dim = c(2, 3))), "Image must be a 3D array")
  expect_error(to_channels_last(matrix(1:6, 2, 3)), "Image must be a 3D array")
  
  # Test raster validation
  expect_error(from_raster("not_a_raster"), "Input must be a Raster object")
})