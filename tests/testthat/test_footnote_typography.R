context("Footnote typography")

test_that("Valid typography passes", {
  expect_null(check_footnote_typography("valid-footnote-typography.tex"))
})

test_that("Invalid stops", {
  expect_error(check_footnote_typography("invalid-footnote-typography.tex"))
})
