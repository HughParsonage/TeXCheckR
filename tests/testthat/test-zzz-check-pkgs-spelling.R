context("(Package spelling)")

test_that("No misspelled words", {
  result <- devtools::spell_check(pkg = "../..",
                                  ignore = c("RStudio", "Rnw", "initialisms",
                                             "regex", "Unbreaking",
                                             "linting", "QXEF", "tex",
                                             "perl", "knitr", "Grattan", 
                                             "TeX", "ary", "biber", "unescaped"))
  expect_equal(length(result), 0)
  
})