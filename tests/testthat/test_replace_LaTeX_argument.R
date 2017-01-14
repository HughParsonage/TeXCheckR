context("replace_LaTeX_argument")

test_that("replace_LaTeX_argument single value", {
  x <- readLines("./SchoolFunding/SchoolFunding.tex")
  x <- grep("Changes in spending over the last decade ", x, fixed = TRUE, value = TRUE)

  out <- replace_LaTeX_argument(x, "label", replacement = "QQ")
  expect_equal(out, "\\subsection{Changes in spending over the last decade }\\label{QQ}")
})

test_that("nth argument", {
  expect_equal(replace_nth_LaTeX_argument("\\begin{smallbox}{Title}{box:some-key}",
                                         fixed = FALSE,
                                         command_name = "\\\\begin.(?:(?:(?:very)?small)|(?:big))box[*]?[}]",
                                         n = 2L,
                                         replacement = "box:key"),
               "\\begin{smallbox}{Title}{box:key}")
})

test_that("nth argument when command absent", {
  expect_equal(replace_nth_LaTeX_argument(c("\\begin{smallbox}{Title}{box:some-key}", "\\somecommand{a}{bc}"),
                                          fixed = FALSE,
                                          command_name = "\\\\begin.(?:(?:(?:very)?small)|(?:big))box[*]?[}]",
                                          n = 2L,
                                          replacement = "box:key"),
               c("\\begin{smallbox}{Title}{box:key}", "\\somecommand{a}{bc}"))
})
