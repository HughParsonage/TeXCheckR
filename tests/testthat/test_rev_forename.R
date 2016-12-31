context("Reverse forename surname")

test_that("rev forename surname works on bibtex-like entry", {
  expect_equal(rev_forename_surname_bibtex("John Daley and Brendan Coates and Ludwig van Beethoven"),
               "Daley, John and Coates, Brendan and Beethoven, Ludwig van")
  expect_equal(rev_forename_surname_bibtex("John Daley and Coates, Brendan and Ludwig van Beethoven"),
               "Daley, John and Coates, Brendan and Beethoven, Ludwig van")
  expect_equal(rev_forename_surname_bibtex("J. Daley and Coates, B and Ludwig van Beethoven"),
               "Daley, J. and Coates, B and Beethoven, Ludwig van")
  expect_equal(rev_forename_surname_bibtex("J Daley and B Coates and L van Beethoven"),
               "Daley, J and Coates, B and Beethoven, L van")
})
