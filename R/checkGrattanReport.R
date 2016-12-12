#' Check Grattan Report
#'
#' @param path Path to search for the filename.
#' @param file If non-NULL, the file is used, disregarding other LaTeX files in \code{path}.
#' @param ignore.list A list of lines to ignore for each cite.
#' @return Called for its side-effect.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else


checkGrattanReport <- function(path = ".",
                               file = NULL,
                               ignore.list = list(cite_pagrefs = NULL,
                                                  dashes = NULL,
                                                  footnote_typography = NULL,
                                                  repetitive_xrefs = NULL,
                                                  sentence_ending_periods = NULL)){
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))

  if (is.null(file)){
    filename <- list.files(pattern = "\\.Rnw$")
    if (length(filename) == 0L){
      filename <- list.files(pattern = "\\.tex$")
    }

    if (length(filename) != 1L){
      stop("Multiple LaTeX files in directory. Specify using file = ")
    } else {
      filename <- filename[[1]]
    }
  } else {
    filename <- file
  }

  cat("cite pagerefs", "\n")
  check_cite_pagerefs(filename)

  cat("Checking dashes...")
  check_dashes(filename)
  cat("\n", "Checking footnote typography...")
  check_footnote_typography(filename, ignore.lines = ignore.list$footnote_typography)

  cat("\n", "Checking repetitive xrefs...")
  check_repetitive_xrefs(filename)

  cat("\n", "Checking sentence-ending periods...")
  check_sentence_ending_periods(filename)

  cat("\n", "Checking spelling...")
  check_spelling(filename)
}
