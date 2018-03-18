context("Reports")

test_that("Housing affordability", {
  skip_on_cran()
  ha <- file.path(tempdir(), "h")
  hutils::provide.dir(ha)
  get_wd <- getwd()
  setwd(ha)
  download.file(url = "https://github.com/grattan/zzz-2018-Housing-affordability/zipball/master",
                mode = "wb",
                destfile = "ha.zip")
  unzip("ha.zip", exdir = ".")
  setwd(grep("grattan-zzz-2018-Housing-affordability", 
             list.dirs(), 
             fixed = TRUE, 
             value = TRUE))
  expect_null(check_spelling("Report.tex"))
  
  Report <- read_tex_document("Report.tex")
  footnotes <- extract_LaTeX_argument(Report, "footnote")
  expect_equal(footnotes[262][["extract"]], 
               "For example, at Ormond and Moonee Ponds.")
  
  expect_identical(footnotes[69L][["char_no_max"]], 103841L)
  
  
  
  setwd(get_wd)
})

