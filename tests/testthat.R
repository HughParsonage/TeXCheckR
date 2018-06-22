library(testthat)
library(TeXCheckR)

if (is_testing() && getOption("TeXCheckR.messages", TRUE)) {
  message('Set\n\toptions("TeXCheckR.messages" = FALSE)\n\t to tidy test() output.')
}

test_check("TeXCheckR")
