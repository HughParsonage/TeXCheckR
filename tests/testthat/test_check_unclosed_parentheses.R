context("Unclosed parentheses")

test_that("Closed parentheses NULL", {
  expect_null(check_unclosed_parentheses(tex_lines = "This (should) work."))
})

test_that("Unclosed parentheses error", {
  expect_error(check_unclosed_parentheses(tex_lines = "(This should error"), 
               regexp = "parenthesis that does not close")
})

test_that("Unopened parenthesis", {
  options("TeXCheckR.halt_on_error" = FALSE)
  expect_output(check_unclosed_parentheses(tex_lines = "This parenthesis) should have been opened"),
                regexp = "Unexpected closing parenthesis.")
  options("TeXCheckR.halt_on_error" = TRUE)
  expect_error(check_unclosed_parentheses(tex_lines = "This parenthesis) should have been opened"),
               regexp = "Unexpected closing parenthesis.")
})

