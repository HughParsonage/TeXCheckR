context("check biber") 

test_that("Couldn't find an entry for", {
  get_wd <- getwd()
  setwd("check-biber/lost-entry/")
  invisible(system2(command = "pdflatex", c("-draftmode", "a.tex"), stdout = TRUE))
  invisible(system2("biber", args = c("--onlylog",  "a"), stdout = TRUE))
  expect_error(check_biber(), "Biber emitted a warning")
  setwd(get_wd)
})

test_that("No journal title", {
  get_wd <- getwd()
  setwd("check-biber/no-journal-title/")
  invisible(system2(command = "pdflatex", c("-draftmode", "a.tex"), stdout = TRUE))
  invisible(system2("biber", args = c("--onlylog -V",  "a"), stdout = TRUE))
  expect_error(check_biber())
  setwd(get_wd)
})

