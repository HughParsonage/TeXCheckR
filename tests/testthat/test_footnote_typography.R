context("Footnote typography")

test_that("Valid typography passes", {
  expect_null(check_footnote_typography("valid-footnote-typography.tex"))
})
