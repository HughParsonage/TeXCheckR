context("lint_bib")

test_that("Correctly tidies known input", {
  # skip("Not supported in R 3.5.0.")
  
  if (file.exists("lint_bib_out.bib")) file.remove("lint_bib_out.bib")
  lint_bib("lint_bib_in.bib", "lint_bib_out.bib")
  expect_identical(readLines("lint_bib_out.bib", encoding = "UTF-8"),
                   readLines("lint_bib_out_correct.bib", encoding = "UTF-8"))
})

