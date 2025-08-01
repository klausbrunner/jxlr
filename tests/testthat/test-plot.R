test_that("plot_to_jxl works with base R plots", {
  skip_if_not_installed("png")

  temp_file <- tempfile(fileext = ".jxl")

  # Test base R plot
  plot_to_jxl(temp_file, plot(1:10, 1:10), width = 4, height = 3, quality = 80)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 100) # Should have some content

  # Should be readable as JXL
  img <- read_jxl(temp_file)
  expect_equal(length(dim(img)), 3)
  expect_equal(dim(img)[1], 4 * 300) # width * dpi
  expect_equal(dim(img)[2], 3 * 300) # height * dpi
  expect_equal(dim(img)[3], 4) # RGBA

  unlink(temp_file)
})

test_that("plot_to_jxl handles errors gracefully", {
  skip_if_not_installed("png")

  temp_file <- tempfile(fileext = ".jxl")

  # Test error in plotting expression
  expect_error(plot_to_jxl(temp_file, stop("test error")), "test error")

  # Device should be properly closed even after error
  expect_equal(as.numeric(dev.cur()), 1) # Should be back to null device

  # Temp file should be cleaned up
  expect_false(file.exists(temp_file))
})

test_that("plot_to_jxl works with complex expressions", {
  skip_if_not_installed("png")

  temp_file <- tempfile(fileext = ".jxl")

  # Test complex plotting expression
  plot_to_jxl(temp_file,
    {
      par(mfrow = c(1, 2))
      plot(1:5, 1:5, main = "Plot 1")
      plot(5:1, 1:5, main = "Plot 2")
    },
    width = 8,
    height = 4
  )

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 200)

  unlink(temp_file)
})
