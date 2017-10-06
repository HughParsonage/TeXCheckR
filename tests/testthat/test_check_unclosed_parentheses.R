context("Unclosed parentheses")

test_that("Closed parentheses NULL", {
  expect_null(check_unclosed_parentheses("This (should) work."))
})

test_that("Unclosed parentheses error", {
  expect_error(check_unclosed_parentheses("(This should error"), 
               regexp = "parenthesis that does not close")
})
