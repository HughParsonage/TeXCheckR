context("Validate bibliography")

test_that("Bills of Parliament", {
  expect_error(validate_bibliography(file = "./validate-bib/invalid-Bill.bib"),
               regexp = "Bills? of Parliament")
  expect_null(validate_bibliography(file = "./validate-bib/valid-Bill.bib"))
})

test_that("Duplicate fields noticed", {
  expect_error(fread_bib("./validate-bib/dup_fields.bib"), 
               regexp = "Duplicate fields found in RMS2010Hunter")
})

test_that("Duplicate entries error", {
  expect_error(any_bib_duplicates("./validate-bib/dup_entries.bib"), 
               regexp = "[Dd]uplicate entries in bibliography")
  expect_error(any_bib_duplicates("./validate-bib/dup_entries-2.bib"), 
               regexp = "[Dd]uplicate entries in bibliography")
})

test_that("Broken fields detected", {
  expect_error(validate_bibliography(file = "./validate-bib/field-broken-over2lines.bib"), 
               regexp = "which is neither a key, nor field")
})
