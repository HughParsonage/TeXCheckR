context("Validate bibliography")

test_that("Bills of Parliament", {
  expect_error(validate_bibliography(file = "./validate-bib/invalid-Bill.bib"),
               regexp = "Bills? of Parliament")
  expect_null(validate_bibliography(file = "./validate-bib/valid-Bill.bib"))
})

test_that("Duplicate fields noticed", {
  expect_error(fread_bib("./tests/validate-bib/dup_fields.bib"), 
               regexp = "Duplicate fields found in RMS2010Hunter")
})
