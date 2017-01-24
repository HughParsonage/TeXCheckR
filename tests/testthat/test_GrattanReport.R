context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  if (!dir.exists("RELEASE")) dir.create("RELEASE")
  if (file.exists("./RELEASE/SchoolFunding.pdf")) file.remove("./RELEASE/SchoolFunding.pdf")
  
  expect_output_file(checkGrattanReport(path = "./SchoolFunding", compile = TRUE, final = TRUE, release = TRUE),
                     "./RELEASE/SchoolFunding.pdf")
  
})
