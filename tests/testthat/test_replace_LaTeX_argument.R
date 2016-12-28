context("replace_LaTeX_argument")

test_that("replace_LaTeX_argument single value", {
  x <- readLines("SchoolFunding.tex")
  x <- grep("Changes in spending over the last decade ", x, fixed = TRUE, value = TRUE)

  out <- replace_LaTeX_argument(x, "label", replacement = "QQ")
  expect_equal(out, "\\subsection{Changes in spending over the last decade }\\label{QQ}")
})
