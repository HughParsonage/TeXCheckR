context("Check xrefs")

test_that("Check case-sensitivity", {
  expect_error(check_xrefs("./check-xrefs/case-sensitive.tex"))
})

test_that("Literal xrefs are detected", {
  expect_null(check_literal_xrefs("./check-xrefs/no-literals-xrefs.tex"))
  expect_error(check_literal_xrefs("./check-xrefs/literals-xrefs.tex"),
               regexp = "Hard-coded xref")
})

