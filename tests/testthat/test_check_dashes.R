context("Check dashes")

test_that("Does not error in math mode", {
  expect_null(check_dashes(filename = "./check-dashes/ok-despite-math.tex"))
})

test_that("Errors if hyphen wrongly typed", {
  expect_error(check_dashes(filename = "./check-dashes/bad-hyphen.tex"),
               regexp = "[Hh]yphen")
})
