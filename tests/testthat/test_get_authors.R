context("Get authors")

test_that("Authors (any order) found in SchoolFunding.tex", {
  expect_equal(sort(get_authors("./SchoolFunding/SchoolFunding.tex")),
               c("Carmela Chivers", "Julie Sonnemann", "Kate Griffiths", "Peter Goss"))
})
