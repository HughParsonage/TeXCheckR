context("fread_bib") 

test_that("fread_bib output", {
  bibDT <- fread_bib("./fread-bib/1.bib")
  
  expect_equal(bibDT[1][["key"]], "AG-2016-Super-splitting-laws-FAQ")
  expect_equal(bibDT[field == "year"][["value"]], "2016")
  
  bibDT_w_braces <- fread_bib("./fread-bib/1.bib", strip.braces = FALSE)
  expect_equal(bibDT_w_braces[line_no == "2"][["field"]], "author")
  expect_equal(bibDT_w_braces[line_no == "2"][["value"]],
               "{{Attorney-General's Department}}")
})

test_that(".bib_expected", {
  tempf <- tempfile()
  file.create(tempf)
  expect_warning(fread_bib(tempf), 
                 regexp = "File extension is not '.bib'.",
                 fixed = TRUE)
  expect_message(fread_bib(tempf, .bib_expected = FALSE),
                 regexp = "Returning empty")
  
  # XOR
  tempf <- tempfile(fileext = ".bib")
  file.create(tempf)
  expect_warning(fread_bib(tempf, .bib_expected = FALSE), 
                 regexp = "File extension is not '.bib'.",
                 fixed = TRUE)
  expect_message(fread_bib(tempf, .bib_expected = TRUE),
                 regexp = "Returning empty")
})
