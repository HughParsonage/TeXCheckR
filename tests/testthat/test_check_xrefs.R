context("Check xrefs")

test_that("Check case-sensitivity", {
  expect_error(check_xrefs("./check-xrefs/case-sensitive.tex"))
})

