context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  if (!dir.exists("./SchoolFunding/RELEASE")){
    dir.create("./SchoolFunding/RELEASE")
  }
  
  if (file.exists("./SchoolFunding/RELEASE/SchoolFunding.pdf")){
    file.remove("./SchoolFunding/RELEASE/SchoolFunding.pdf")
  }
  
  checkGrattanReport(path = "./SchoolFunding", compile = TRUE, final = TRUE, release = TRUE)
  
  expect_true(file.exists("./SchoolFunding/RELEASE/SchoolFunding.pdf"))
  
})
