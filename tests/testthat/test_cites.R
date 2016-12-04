context("Citation formatting")

test_that("Postnotes detected", {
  expect_error(check_cite_pagerefs("cite-no-postnote.tex"), regexp = "postnote")
  expect_error(check_cite_pagerefs("cite-p-in-postnote.tex"), regexp = "p in postnote")
})
