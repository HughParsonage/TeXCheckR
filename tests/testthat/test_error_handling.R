context("Error handling")

test_that("Error messages make sense", {
  expect_error(checkGrattanReport(pre_release = FALSE,
                                  release = TRUE),
               regexp = "release = TRUE but pre_release and compile are not both TRUE also")

  expect_error(checkGrattanReport(pre_release = TRUE,
                                  compile = FALSE),
               regexp = "pre_release = TRUE but compile = FALSE")

})
