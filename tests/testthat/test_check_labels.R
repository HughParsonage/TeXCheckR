context("check_labels")

test_that("Stops on absent prefixes", {
  expect_error(check_labels("check-labels/label-without-prefix.tex"), 
               regexp = "must contain a prefix")
})

test_that("Stops when chapter prefixes wrong", {
  expect_error(check_labels("./check-labels/chapter-with-wrong-prefix.tex"))
  expect_error(check_labels("./check-labels/chapter-without-label.tex"))
})

test_that("Stops when chapref not used", {
  expect_error(check_labels("./check-labels/chapter-using-Vref.tex"), regexp = "Chapref")
})

test_that("Stops when Chapref is unlinked", {
  expect_error(check_all_figs_tbls_refd("./check-labels/Chapref-not-linked.tex"), regexp = "empty cross-reference")
})
