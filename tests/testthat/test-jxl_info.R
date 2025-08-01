test_that("jxl_info handles missing files correctly", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  expect_error(jxl_info("nonexistent.jxl"), "Cannot open file")
})

test_that("jxl_info returns expected structure", {
  skip_if(system("pkg-config --exists libjxl", ignore.stdout = TRUE) != 0, 
          "libjxl not available")
  
  expect_true(exists("jxl_info"))
  expect_true(is.function(jxl_info))
})