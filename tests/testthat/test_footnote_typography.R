context("Footnote typography")

test_that("Valid typography passes", {
  expect_null(check_footnote_typography("valid-footnote-typography.tex"))
})

test_that("Invalid typography stops", {
  expect_error(check_footnote_typography("invalid-footnote-typography.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/doesnt-end-with-period.tex"),
               regexp = "does not end with full stop")
  expect_error(check_footnote_typography("./fnote-typogr/full-stop-after.tex"),
               regexp = "[fF]ull stop after")
})

test_that("Space before footnotes.", {
  # a1 b1 b2  Test  Expect  Description
  #  T  T  T  1     PASS    Tabbed footnote on own line: b2 protects
  #  T  T  F  2     FAIL    Tabbed footnote without protection.
  #  T  F  T  3     FAIL    Ordinary space (and txt) before fn % irrelevant -- protective space has no effect
  #  T  F  F  4     FAIL    Ordinary space (and txt) before fn: lazy dog \footnote
  #  F  T  T  5     PASS    Non-tabbed footnote \footnote at start of text: b2 protects
  #  F  T  F  6     FAIL    Non-tabbed footnote without protection
  #  F  F  T  7     PASS    No footnote
  #  F  F  F  8     PASS    No footnote
  expect_null(check_footnote_typography("./fnote-typogr/1.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/2.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/3.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/4.tex"))
  expect_null(check_footnote_typography("./fnote-typogr/5.tex"))
  expect_error(check_footnote_typography("./fnote-typogr/6.tex"))
  expect_null(check_footnote_typography("./fnote-typogr/7.tex"))
  expect_null(check_footnote_typography("./fnote-typogr/8.tex"))
  expect_null(check_footnote_typography("./fnote-typogr/isOK.tex"))
})
