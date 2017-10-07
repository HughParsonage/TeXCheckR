context("inputs of")

test_that("Inputs of (no preamble)", {
  inputs_of_nopreamble <- inputs_of("extract/road-congestion.tex")
  expect_equal(inputs_of_nopreamble, "tex/MEL-commutes-map-legend-tbl.tex")
  expect_equal(inputs_of("extract/road-congestion.tex", exclude.preamble = FALSE),
               c("tex/preamble.tex", "tex/MEL-commutes-map-legend-tbl.tex"))
  expect_equal(inputs_of("extract/road-congestion.tex", exclude.preamble = FALSE, append.tex = FALSE),
               c("tex/preamble", "tex/MEL-commutes-map-legend-tbl"))
})
