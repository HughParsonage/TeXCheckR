context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  checkGrattanReport(file = "SchoolFunding.tex")
})
