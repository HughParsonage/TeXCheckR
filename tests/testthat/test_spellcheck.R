context("Spellchecker")

test_that("School funding report checks out", {
  expect_null(check_spelling("./SchoolFunding/SchoolFunding.tex", 
                             known.correct = c("SRS", "SE.XPD.TOTL.GD.XS", "WDI", "SSNP", "underfunded", "overfund[a-z]*", "NMS", "WPI", "DET", "phas", "NP", "SATs", "ENG", "th", "stds", "RCTs", "CAGR"), ignore.lines = 1551))
})

test_that("Check spelling of multiple input document", {
  expect_error(check_spelling("./spellcheck_multi_input/spellcheck_multi_input.tex"),
               regexp = "failed on above line")
})

test_that("Abbreviations", {
  expect_error(check_spelling("spellcheck-abbrevs.tex"))
})

test_that("Add to dictionary, ignore spelling in", {
  expect_error(check_spelling("./spelling/add_to_dictionary-wrong.tex"), regexp = "[Ss]pellcheck failed")
  expect_error(check_spelling("./spelling/ignore_spelling_in-wrong.tex", pre_release = FALSE), regexp = "[Ss]pellcheck failed")
  
  expect_null(check_spelling("./spelling/add_to_dictionary-ok.tex"))
  expect_null(check_spelling("./spelling/ignore_spelling_in-ok.tex", pre_release = FALSE))
  
  expect_error(check_spelling("./spelling/ignore_spelling_in-ok.tex"), regexp = "pre_release = TRUE")
})
