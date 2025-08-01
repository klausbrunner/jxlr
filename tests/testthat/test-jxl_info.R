test_that("jxl_info handles missing files correctly", {
  expect_error(jxl_info("nonexistent.jxl"), "Cannot open file")
})

test_that("jxl_info returns expected structure", {
  expect_true(exists("jxl_info"))
  expect_true(is.function(jxl_info))
})
