context("check preamble")

test_that("todonotes even if disabled not acceptable", {
  expect_error(check_preamble("./check-preamble/todonotes.tex", final = TRUE))
  expect_error(check_preamble("./check-preamble/todonotes-disable.tex", final = TRUE))
  cat("\n")
})

test_that("Working paper inconsistency", {
  expect_error(check_preamble("./check-preamble/working-paper/working-paper-with-report-not.tex"), 
               regexp = "ReportOrWorkingPaper set to .Working Paper. but statement")
  expect_error(check_preamble("./check-preamble/working-paper/report-with-working-paper.tex"), 
               regexp = "ReportOrWorkingPaper not set to .Working Paper. but")
})
