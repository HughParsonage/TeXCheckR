context("check all figs tbls refd")

test_that("Error when not refd", {
  expect_error(check_all_figs_tbls_refd("./check-all-figs-tbls-refd/fig_not_refd.tex"))
})

test_that("Error when not labeled", {
  expect_error(check_all_figs_tbls_refd("./check-all-figs-tbls-refd/fig-not-labeled.tex"))
})
