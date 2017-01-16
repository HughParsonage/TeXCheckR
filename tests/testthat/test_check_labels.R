context("check_labels")

test_that("Stops on absent prefixes", {
  expect_error(check_labels("check-labels/label-without-prefix.tex"))
})

test_that("Stops when chapter prefixes wrong", {
  expect_error(check_labels("./check-labels/chapter-with-wrong-prefix.tex"))
  expect_error(check_labels("./check-labels/chapter-without-label.tex"))
})

test_that("Stops when chapref not used", {
  expect_error(check_labels("./check-labels/chapter-using-Vref.tex"))
})

