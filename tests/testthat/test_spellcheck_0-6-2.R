context("spellcheck")

test_that("grattanReporter #113", {
  # i.e. respect ignore_spelling_in_nth for inputs
  expect_error(check_spelling("spelling/ignore_113.tex"), 
               regexp = "failed")
  expect_null(check_spelling("spelling/ignore_113.tex", 
                             ignore_spelling_in_nth = list("foo" = 1:2)))
})

