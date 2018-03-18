context("Reports")

test_that("Housing affordability", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("biber")))
  library(data.table)
  h <- sample(letters, size = 1)
  ha <- file.path(tempdir(), h)
  hutils::provide.dir(ha)
  get_wd <- getwd()
  setwd(ha)
  dl_status <- 
    download.file(url = "https://github.com/grattan/zzz-2018-Housing-affordability/zipball/master",
                  mode = "wb",
                  cacheOK = FALSE,
                  destfile = "ha.zip", 
                  quiet = TRUE)
  unzip("ha.zip", exdir = ".")
  setwd(grep("grattan-zzz-2018-Housing-affordability", 
             list.dirs(), 
             fixed = TRUE, 
             value = TRUE))
  expect_null(check_spelling("Report.tex"))
  
  Report <- read_tex_document("Report.tex")
  Report.tex <- tempfile(fileext = ".tex")
  write_lines(Report, Report.tex)
  footnotes <- extract_LaTeX_argument(Report, "footnote")
  expect_equal(footnotes[262][["extract"]], 
               "For example, at Ormond and Moonee Ponds.")
  
  expect_identical(footnotes[69L][["char_no_max"]], 103841L)
  
  expect_null(check_cite_pagerefs(Report.tex))
  tempf_consecutive <- tempfile(pattern = "consecutive",
                                fileext = ".txt")
  system("pdflatex -interaction=batchmode Report.tex")
  system("biber Report")
  system("pdflatex -interaction=batchmode Report.tex")
  consecutive_outfile <- tempfile()
  expect_error(check_consecutive_words(latex_file = "Report.tex",
                                       outfile = consecutive_outfile))
  
  # Separate land tax schedules could be introduced for residential and
  # commercial land, with residential land paying a low flat rate and
  # commercial land remaining subject to a progressive land tax schedule
  # in order to to prevent windfall gains to large existing commercial
  # landholders.
  expect_true("commercial land, with residential land paying a low flat rate and" %chin% trimws(readLines(consecutive_outfile)))
  expect_true("commercial land remaining subject to a progressive land tax schedule" %chin% trimws(readLines(consecutive_outfile)))
  
  
  setwd(get_wd)
})

