test_that("JXL animation functions work", {
  frame1 <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
  frame2 <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
  frame3 <- array(runif(5 * 5 * 3), dim = c(5, 5, 3))
  frames <- list(frame1, frame2, frame3)

  # Test write_jxl_anim to raw vector
  anim_raw <- write_jxl_anim(frames, durations = c(100, 200, 150), quality = 80)
  expect_s3_class(anim_raw, "jxl_anim")
  expect_type(anim_raw, "raw")

  # Test read_jxl_anim from raw vector
  anim_read <- read_jxl_anim(anim_raw, numeric = TRUE)
  expect_s3_class(anim_read, "jxl_anim")
  expect_equal(length(anim_read), 3)
  expect_equal(anim_read$width, 5)
  expect_equal(anim_read$height, 5)
  expect_equal(length(anim_read$durations), 3)

  # Test indexing
  expect_equal(length(anim_read[1:2]), 2)
  expect_equal(dim(anim_read[[1]]), c(5, 5, 4)) # RGBA output

  # Test file operations
  temp_file <- tempfile(fileext = ".jxl")
  write_jxl_anim(frames, temp_file, durations = c(100, 200, 150))
  expect_true(file.exists(temp_file))

  anim_from_file <- read_jxl_anim(temp_file)
  expect_equal(length(anim_from_file), 3)
  expect_equal(anim_from_file$width, 5)
  expect_equal(anim_from_file$height, 5)

  unlink(temp_file)
})

test_that("JXL animation handles RGBA frames", {
  frame1 <- array(runif(4 * 4 * 4), dim = c(4, 4, 4))
  frame2 <- array(runif(4 * 4 * 4), dim = c(4, 4, 4))
  frames <- list(frame1, frame2)

  anim_raw <- write_jxl_anim(frames, durations = c(300, 400), quality = 90)
  expect_s3_class(anim_raw, "jxl_anim")

  anim_read <- read_jxl_anim(anim_raw)
  expect_equal(length(anim_read), 2)
  expect_equal(dim(anim_read[[1]]), c(4, 4, 4))
})

test_that("JXL animation parameter validation works", {
  frame <- array(runif(3 * 3 * 3), dim = c(3, 3, 3))

  expect_error(write_jxl_anim(list()), "No frames provided")
  expect_error(write_jxl_anim(list(frame), quality = -1), "quality must be between 0 and 100")
  expect_error(write_jxl_anim(list(frame), effort = 10), "effort must be an integer between 1 and 9")

  anim <- write_jxl_anim(list(frame, frame), durations = 200)
  anim_read <- read_jxl_anim(anim)
  expect_equal(anim_read$durations, c(200, 200))
})
