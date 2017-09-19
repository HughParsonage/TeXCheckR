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

test_that("Optional argument", {
  out <- extract_LaTeX_argument("See \\textcite[Ante][Post]{Knuth1} [if you want].",
                                command_name = "textcite", 
                                n = 2L, 
                                optional = TRUE)
  expect_equal(out[["extract"]], "Post")
  
  out <- extract_LaTeX_argument("See \\textcite[Ante][[ex] Post]{Knuth1} [if you want].",
                                command_name = "textcite", 
                                n = 2L, 
                                optional = TRUE)
  
  expect_equal(out[["extract"]], "[ex] Post")
})

test_that("Multi-line", {
  out <- extract_LaTeX_argument(c("This \\footnote{", "ends", "here.}"), "footnote")
})
  



