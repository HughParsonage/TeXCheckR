context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  checkGrattanReport(path = "./SchoolFunding")
})
