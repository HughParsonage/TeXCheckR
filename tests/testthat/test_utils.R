context("utils")

test_that("Seq_union always length one", {
  expect_equal(Seq_union(1, 5), 1:5)
  expect_equal(Seq_union(1, c(5, 10)), 1:10)
  expect_equal(Seq_union(c(73, 100), c(83, 105)), c(73:83, 100:105))
  expect_equal(Seq_union(c(1, 10), c(9, 19)), 1:19)
})


test_that("stringi replacements", {
  skip_if_not_installed("stringi")
  Lines <- readLines("Example-report.Rnw")
  expect_identical(stri_locate_first_fixed_no_stringi(Lines, " - "), 
                   stri_locate_first_fixed(Lines, " - "))
})

