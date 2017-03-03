context("Get authors")

test_that("Authors (any order) found in SchoolFunding.tex", {
  expect_equal(sort(get_authors("./SchoolFunding/SchoolFunding.tex")),
               c("Carmela Chivers", "Julie Sonnemann", "Kate Griffiths", "Peter Goss"))
})

test_that("Authors (any order) found in input: tex/acknowledgements", {
  expect_equal(sort(get_authors("./get-authors/input.tex")),
               c("Hal Swerissen", "Stephen Duckett", "Trent Wiltshire"))
})

test_that("Editorial authors not returned", {
  expect_equal(get_authors("./get-authors/editorial-authors.tex", include_editors = FALSE), 
               c("Stephen Duckett", "Hal Swerissen"))
})

test_that("Additional authors are returned", {
  expect_equal(get_authors("./get-authors/add-authors-to-citation.tex", include_editors = FALSE), 
               c("Stephen Duckett", "Hal Swerissen", "Lemony Snicket"))
})
