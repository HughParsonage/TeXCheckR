context("UTF8 to LaTeX")

test_that("Conversions", {
  skip_if_not_installed("readr")
  y <- readr::read_lines("SchoolFunding/2016-SchoolFunding.bib")
  z <- utf8ToLaTeX(z)
  expect_identical(z, utf8ToLaTeX("SchoolFunding/2016-SchoolFunding.bib"))
  exp <-
    c("  title       = {Improving Impact Studies of Teachers' Professional Development: Toward Better Conceptualizations and Measures},", 
      "  journal     = {G\\\"{o}teborg Utbildningsf\\\"{o}rvaltningen},", 
      "  author      = {Tina Fernstr\\\"{o}m},", "  author      = {Ben Jensen and Am\\'{e}lie Hunter and Julie Sonnemann and Tracy Burns},", 
      "  author      = {Ben Jensen and Am\\'{e}lie Hunter},", "  author      = {Jensen, Ben and Molyneux, Katherine and Hunter, Am\\'{e}lie},"
    )
  expect_identical(utf8ToLaTeX(y)[y != utf8ToLaTeX(y)], 
                   exp)
  
})



