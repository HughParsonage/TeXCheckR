context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  if (!dir.exists("./SchoolFunding/RELEASE")){
    dir.create("./SchoolFunding/RELEASE")
  }
  
  if (file.exists("./SchoolFunding/RELEASE/SchoolFunding.pdf")){
    file.remove("./SchoolFunding/RELEASE/SchoolFunding.pdf")
  }
  
  checkGrattanReport(path = "./SchoolFunding", compile = TRUE, pre_release = TRUE, release = FALSE)
  
  expect_true(file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf"))
  
})
