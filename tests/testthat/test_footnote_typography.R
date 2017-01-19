context("Footnote typography")

test_that("Valid typography passes", {
  expect_null(check_footnote_typography("valid-footnote-typography.tex"))
})

test_that("Invalid typography stops", {
  expect_error(check_footnote_typography("invalid-footnote-typography.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/doesnt-end-with-period.tex"),
               regexp = "does not end with full stop")
  expect_error(check_footnote_typography("./fnote-typogr/full-stop-after.tex"),
               regexp = "[fF]ull stop after")
})
