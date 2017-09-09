context("Extract arguments")

test_that("Extract textbf", {
  out <- extract_LaTeX_argument("The contents of \\textbf{ABC} is abc.", "textbf")
  expect_equal(out[["extract"]], "ABC")
})

test_that("Extract textcites", {
  out <- extract_LaTeX_argument("Some citation by \\textcites{Knuth194}{Knuth195}", "textcites", n = 2L)
  expect_equal(out[["extract"]], "Knuth195")
})

test_that("Extract nested", {
  out <- extract_LaTeX_argument("What should this be \\emph{Some emphasized text \\emph{Double emphasized} here.}",
                                "emph")
  
  expect_equal(out[["extract"]], 
               c("Some emphasized text \\emph{Double emphasized} here.",
                 "Double emphasized"))
})

test_that("Blank line", {
  out <- extract_LaTeX_argument("", "foo")
  expect_true(is.na(out[["extract"]]))
})
  



