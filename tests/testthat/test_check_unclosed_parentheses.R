context("Unclosed parentheses")

test_that("Closed parentheses NULL", {
  expect_null(check_unclosed_parentheses(tex_lines = "This (should) work."))
})

test_that("Unclosed parentheses error", {
  expect_error(check_unclosed_parentheses(tex_lines = "(This should error"), 
               regexp = "parenthesis that does not close")
})

test_that("Unopened parenthesis", {
  
  # options("TeXCheckR.halt_on_error" = FALSE)
  # the_msg <- 
  #   capture.output(check_unclosed_parentheses(tex_lines = "This parenthesis) should have been opened"))
  # 
  # expect_true(any(grepl("Unexpected closing parenthesis.",
  #                       the_msg, 
  #                       fixed = TRUE)),
  #             info = the_msg)
  # 
  options("TeXCheckR.halt_on_error" = TRUE)
  expect_error(check_unclosed_parentheses(tex_lines = "This parenthesis) should have been opened"),
               regexp = "Unexpected closing parenthesis.")
  
  expect_null(check_unclosed_parentheses(tex_lines = "This is ok a) foo b) bar c) baz."))
  expect_null(check_unclosed_parentheses(tex_lines = "This is ok a) foo b) bar 1) baz. 2) foobaz"))
  expect_null(check_unclosed_parentheses(tex_lines = "This is ok A) foo B) bar 1) baz. 2) foobaz"))
  expect_error(check_unclosed_parentheses(tex_lines = "This is ok a) foo b) bar 1) baz. B) foobaz"))
  expect_error(check_unclosed_parentheses(tex_lines = "This is ok a) foo b) bar 1) baz. B) foobaz"))
  options("TeXCheckR.halt_on_error" = FALSE)
})

