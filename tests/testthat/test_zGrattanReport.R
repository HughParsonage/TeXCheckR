context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  expect_null(checkGrattanReport(path = "./SchoolFunding"))
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

test_that("Engaging-students", {
  skip_on_travis()
  skip_if_not(file.exists('C:/Program Files/gs/gs9.20/bin/gswin64c.exe'))
  Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')
  if (!dir.exists("./Engaging-students/RELEASE")){
    dir.create("./Engaging-students/RELEASE")
  }
  
  if (file.exists("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")){
    file.remove("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")
  }
  
  checkGrattanReport(path = "./Engaging-students/", compile = TRUE, pre_release = TRUE, release = TRUE)
  
  expect_true(file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf"))
  file.remove("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")
})



