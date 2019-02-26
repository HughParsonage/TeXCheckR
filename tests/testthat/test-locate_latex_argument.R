context("test-locate_latex_argument.R")

test_that("Locations", {
  Line1 <- "\\documentclass[11pt,parksip=half*]{scrartcl} "
  Pars1 <- parse_tex(Line1)
  out <- locate_mandatory_LaTeX_argument(parsed_doc = Pars1, command_name = "documentclass")
  expect_true("documentclass" %in% names(out))
  expect_equal(nrow(out), nrow(Pars1))
  expect_equal(out[(documentclass)][["column"]], 
               seq.int(from = nchar("\\documentclass[11pt,parksip=half*]{"),
                       to = nchar("\\documentclass[11pt,parksip=half*]{scrartcl}")))
})

test_that("Issue 73", {
  x <- "State governments should relax planning restrictions (see \\Chaprefrange{subsec:relax-planning-rules-to-allow-more-density-in-inner-and-middle-ring-suburbs-of-our-cities}{subsec:empower-independent-panels-to-determine-more-planning-applications} on pages \\pageref{subsec:relax-planning-rules-to-allow-more-density-in-inner-and-middle-ring-suburbs-of-our-cities}--\\pageref{subsec:empower-independent-panels-to-determine-more-planning-applications})."
  locate_mandatory_LaTeX_argument(x, "Chaprefrange", n = 1L)
  
})

