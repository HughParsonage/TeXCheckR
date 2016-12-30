context("check_biber")

test_that("biber parsed correctly", {
  skip_on_travis()
  skip_on_cran()
  expect_error(check_biber())
})
