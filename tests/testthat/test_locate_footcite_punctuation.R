context("locate_footcite_punctuation")

test_that("Finds correct location", {
  skip_if_not_installed("readr")
  library(readr)
  res <- locate_footcite_punctuation(tex_lines = read_lines("locate_footcite_punctuation/1.tex"))
  expect_equal(nrow(res), 1L)
  expect_equal(res[["line_no"]], 10L)
  expect_equal(res[["column"]], 16L)
  
  
  res <- locate_footcite_punctuation(tex_lines = read_lines("locate_footcite_punctuation/2.tex"),
                                     singular = FALSE)
  expect_equal(nrow(res), 1L)
  expect_equal(res[["line_no"]], 10L)
  expect_equal(res[["column"]], 27L)
  
  res <- locate_footcite_punctuation(tex_lines = read_lines("locate_footcite_punctuation/2.tex"),
                                     singular = TRUE)
  expect_equal(nrow(res), 1L)
  expect_equal(res[["line_no"]], 15L)
  expect_equal(res[["column"]], 43L)
  
  res <- locate_footcite_punctuation(tex_lines = read_lines("locate_footcite_punctuation/3.tex"),
                                     singular = TRUE)
  expect_equal(nrow(res), 2L)
  expect_equal(res[["line_no"]], 15:16)
  expect_equal(res[["column"]], 43:44 - 0:1)
})
