#' Check Grattan Report
#'
#' @param path Path to search for the filename.
#' @param ignore.list A list of lines to ignore for each cite.
#' @return Called for its side-effect.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else


checkGrattanReport <- function(path = ".",
                               ignore.list = list(cite_pagrefs = NULL,
                                                  dashes = NULL,
                                                  footnote_typography = NULL,
                                                  repetitive_xrefs = NULL,
                                                  sentence_ending_periods = NULL)){
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))

  filename <- list.files(pattern = "Rnw$")
  if (length(filename) == 0L){
    filename <- list.files(pattern = "tex$")
  }
  cat("cite pagerefs", "\n")
  check_cite_pagerefs(filename)

  cat("Checking dashes...")
  check_dashes(filename)
  cat("\n", "Checking footnote typography...")
  check_footnote_typography()

  cat("\n", "Checking repetitive xrefs...")
  check_repetitive_xrefs(filename)

  cat("\n", "Checking sentence-ending periods...")
  check_sentence_ending_periods(filename)
}
