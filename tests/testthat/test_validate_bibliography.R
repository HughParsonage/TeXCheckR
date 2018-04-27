context("Validate bibliography")

test_that("Bills of Parliament", {
  expect_error(validate_bibliography(file = "./validate-bib/invalid-Bill.bib"),
               regexp = "Bills? of Parliament")
  expect_null(validate_bibliography(file = "./validate-bib/valid-Bill.bib"))
})

test_that("Duplicate fields noticed", {
  expect_error(any_bib_duplicates("./validate-bib/dup_fields.bib"), 
               regexp = "Duplicate fields found in RMS2010Hunter")
})

test_that("Duplicate entries error", {
  expect_error(any_bib_duplicates("./validate-bib/dup_entries.bib"), 
               regexp = "[Dd]uplicate entries in bibliography")
  expect_error(any_bib_duplicates("./validate-bib/dup_entries-2.bib"), 
               regexp = "[Dd]uplicate entries in bibliography")
})

test_that("Duplicate entry keys", {
  expect_error(any_bib_duplicates("./validate-bib/dup-key.bib"),
               regexp = "Duplicate bib key used")
})

test_that("Broken fields detected", {
  expect_error(validate_bibliography(file = "./validate-bib/field-broken-over2lines.bib"), 
               regexp = "which is neither a key, nor field")
})

test_that("Near-duplicate authors", {
  expect_error(any_bib_duplicates("validate-bib/near-dup-authors.bib", report2console),
               regexp = "Same author used with inconsistent.*case")
})

test_that("Hypercorrected mendeleys", {
  if (isTRUE(getOption("TeXCheckR.halt_on_error"))) {
    expect_error(validate_bibliography(file = "./validate-bib/invalid-hypercorrected.bib"),
                 regexp = "URL contains hypercorrected escapes.",
                 fixed = TRUE)
  } else {
    options("TeXCheckR.halt_on_error" = TRUE)
    expect_error(validate_bibliography(file = "./validate-bib/invalid-hypercorrected.bib"),
                 regexp = "URL contains hypercorrected escapes.",
                 fixed = TRUE)
    options("TeXCheckR.halt_on_error" = FALSE)
  }
  expect_null(validate_bibliography(file = "./validate-bib/valid-hypercorrected.bib"))
})
