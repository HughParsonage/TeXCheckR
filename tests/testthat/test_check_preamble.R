context("check preamble")

test_that("todonotes even if disabled not acceptable", {
  expect_error(check_preamble("./check-preamble/todonotes.tex"))
  expect_error(check_preamble("./check-preamble/todonotes-disable.tex"))
  cat("\n")
})
