context("check all figs tbls refd")

test_that("Error when not refd", {
  expect_error(check_all_figs_tbls_refd("./check-all-figs-tbls-refd/fig_not_refd.tex", compile = TRUE, pre_release = TRUE), 
               regexp = "fig:this-lab-not-refd")
})

test_that("No error when unreferenced label isn't from tbl or fig", {
  expect_null(check_all_figs_tbls_refd("./check-all-figs-tbls-refd/sec_not_refd.tex", compile = TRUE, pre_release = TRUE))
})

test_that("Error when not labeled", {
  expect_error(check_labels("./check-all-figs-tbls-refd/fig-not-labeled.tex", compile = TRUE, pre_release = TRUE))
})

test_that("No error when commented out", {
  expect_null(check_all_figs_tbls_refd("./check-all-figs-tbls-refd/ok-as-fig-in-comment.tex", compile = TRUE, pre_release = TRUE))
})
