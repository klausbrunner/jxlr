test_that("JXL read/write functions work", {
  skip_if_not_installed("png")
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  
  width <- 10
  height <- 10
  channels <- 3
  
  img <- array(runif(width * height * channels), dim = c(width, height, channels))
  
  jxl_raw <- write_jxl(img, target = NULL, quality = 90)
  expect_s3_class(jxl_raw, "jxl")
  expect_type(jxl_raw, "raw")
  
  img_read <- read_jxl(jxl_raw, numeric = TRUE)
  expect_type(img_read, "double")
  expect_equal(dim(img_read), c(width, height, 4)) # RGBA output
  
  temp_file <- tempfile(fileext = ".jxl")
  write_jxl(img, temp_file, quality = 90)
  expect_true(file.exists(temp_file))
  
  img_from_file <- read_jxl(temp_file, numeric = TRUE)
  expect_equal(dim(img_from_file), c(width, height, 4))
  
  img_raw <- read_jxl(temp_file, numeric = FALSE)
  expect_s3_class(img_raw, "rawimg")
  expect_type(img_raw, "raw")
  
  unlink(temp_file)
})

test_that("JXL handles RGBA images", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  width <- 5
  height <- 5
  img <- array(runif(width * height * 4), dim = c(width, height, 4))
  
  jxl_raw <- write_jxl(img, target = NULL, quality = 80)
  expect_s3_class(jxl_raw, "jxl")
  
  img_read <- read_jxl(jxl_raw, numeric = TRUE)
  expect_equal(dim(img_read), c(width, height, 4))
  expect_type(img_read, "double")
})

test_that("JXL quality parameter works", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  img <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
  
  jxl_high <- write_jxl(img, quality = 95)
  jxl_low <- write_jxl(img, quality = 50)
  jxl_lossless <- write_jxl(img, quality = NA)
  
  expect_s3_class(jxl_high, "jxl")
  expect_s3_class(jxl_low, "jxl")
  expect_s3_class(jxl_lossless, "jxl")
  
  expect_true(length(jxl_lossless) >= length(jxl_low))
  
  expect_equal(dim(read_jxl(jxl_high)), c(5, 5, 4))
  expect_equal(dim(read_jxl(jxl_low)), c(5, 5, 4))
  expect_equal(dim(read_jxl(jxl_lossless)), c(5, 5, 4))
})

test_that("JXL error handling works", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  expect_error(read_jxl(raw(10)), "JXL decoder error")
  expect_error(write_jxl(array(1, dim = c(5, 5, 2))), "channels == 3 || channels == 4")
  expect_error(write_jxl(array(1, dim = c(5, 5, 3)), quality = 150), "quality must be between 0 and 100")
  expect_error(write_jxl(array(1, dim = c(5, 5, 3)), effort = 10), "effort must be between 1 and 9")
})

test_that("JXL effort parameter works", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  img <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
  
  jxl_fast <- write_jxl(img, quality = 80, effort = 1)
  jxl_best <- write_jxl(img, quality = 80, effort = 9)
  
  expect_s3_class(jxl_fast, "jxl")
  expect_s3_class(jxl_best, "jxl")
  
  expect_equal(dim(read_jxl(jxl_fast)), c(5, 5, 4))
  expect_equal(dim(read_jxl(jxl_best)), c(5, 5, 4))
})