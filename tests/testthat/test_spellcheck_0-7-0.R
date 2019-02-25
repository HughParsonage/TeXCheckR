context("spellcheck 0.7.0")

test_that("Possessive apostrophes don't trigger false positives", {
  tempf.tex <- tempfile(fileext = ".tex")
  writeLines(c("\\documentclass{article}",
               "% add_to_dictionary: ALP",
               "",
               "\\begin{document}",
               "The ALP's conference was held in Australia.",
               "\\end{document}",
               ""),
             tempf.tex)
  expect_null(check_spelling(tempf.tex))
})
