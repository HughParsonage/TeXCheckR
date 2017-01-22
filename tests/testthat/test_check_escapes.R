context("Check escapes")

test_that("Dollar signs are not allowed.", {
  expect_error(check_escapes("./check-escapes/fail.tex"))
})
