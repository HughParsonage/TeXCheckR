#' Check Grattan Report
#'
#' @param path Path to search for the filename.
#' @param file If non-NULL, the file is used, disregarding other LaTeX files in \code{path}.
#' @param ignore.list A list of lines to ignore for each cite.
#' @return Called for its side-effect.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @importFrom clisymbols symbol
#' @importFrom crayon green red

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

  check_cite_pagerefs(filename)
  cat(green(symbol$tick, "Cite and pagrefs checked.\n"))

  check_dashes(filename)
  cat(green(symbol$tick, "Dashes correctly typed.\n"))

  suppressMessages(check_footnote_typography(filename, ignore.lines = ignore.list$footnote_typography))
  cat(green(symbol$tick, "Footnote typography checked.\n"))

  check_repetitive_xrefs(filename)
  cat(green(symbol$tick, "No repetitive xrefs.\n"))

  check_sentence_ending_periods(filename)
  cat(green(symbol$tick, "Sentence-ending periods ok.\n"))

  check_spelling(filename,
                 known.correct = if (file.exists("./checkGrattanReport/spelling_known_correct.txt")){
                   readLines("./checkGrattanReport/spelling_known_correct.txt", warn = FALSE, encoding = "UTF-8")
                 } else {
                   NULL
                 })
  cat(green(symbol$tick, "Spellcheck complete.\n"))
}
