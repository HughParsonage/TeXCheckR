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
  expect_error(figs_tbls_unrefd("./check-labels/Chapref-not-linked.tex"), regexp = "empty cross-reference")
})

test_that("Labels with space after", {
  temp_file <- tempfile(pattern = "labels",
                        fileext = ".tex")
  
  Cat <- function(...) base::cat(..., file = temp_file, append = TRUE, sep = "\n")
  
  Cat("\\documentclass{article}")
  Cat("")
  Cat("")
  Cat("\\begin{document}")
  Cat("Some figure.")
  Cat("\\begin{figure}")
  Cat("\\caption{Some label}\\label{fig:A B}")
  Cat("\\includegraphics{some-figure}")
  Cat("\\end{figure}")
  Cat("")
  Cat("\\end{document}")
  Cat("")
  
  expect_error(check_labels(temp_file)) 
})
