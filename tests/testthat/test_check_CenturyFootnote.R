context("CenturyFootnote")

test_that("Passes on valid input", {
  expect_null(check_CenturyFootnote("./check-CenturyFootnote/correctly-placed/"))
})

test_that("Errors when expected.", {
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/fewer-than-100/"), regexp = "fewer than 100")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/not-used/"), regexp = "not used")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/too-early/", strict = TRUE), regexp = "CenturyFootnote fell")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/too-late/", strict = TRUE), regexp = "occurs after the 100th")
})
