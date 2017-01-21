context("check preamble")

test_that("todonotes even if disabled not acceptable", {
  expect_error(check_preamble("./check-preamble/todonotes.tex", final = TRUE))
  expect_error(check_preamble("./check-preamble/todonotes-disable.tex", final = TRUE))
  cat("\n")
})
