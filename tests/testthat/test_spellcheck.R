context("Spellchecker")

test_that("School funding report checks out", {
  expect_null(check_spelling("./SchoolFunding/SchoolFunding.tex", 
                             known.correct = c("SRS", "SE.XPD.TOTL.GD.XS", "WDI", "SSNP", "underfunded", "overfund[a-z]*", "NMS", "WPI", "DET", "phas", "NP", "SATs", "ENG", "th", "stds", "RCTs", "CAGR"), ignore.lines = 1551))
})

test_that("Check spelling of multiple input document", {
  expect_error(check_spelling("./spellcheck_multi_input/spellcheck_multi_input.tex"),
               regexp = "failed on above line")
})
