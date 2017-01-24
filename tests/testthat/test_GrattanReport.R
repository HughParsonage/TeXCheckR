context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  checkGrattanReport(path = "./SchoolFunding", compile = TRUE)
})

test_that("SchoolFunding.tex", {
  skip_on_travis()
  if (!dir.exists("./SchoolFunding/PRE-RELEASE")){
    dir.create("./SchoolFunding/PRE-RELEASE")
  }
  
  if (file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf")){
    file.remove("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf")
  }
  
  checkGrattanReport(path = "./SchoolFunding", compile = TRUE, pre_release = TRUE, release = FALSE)
  
  expect_true(file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf"))
  
})
