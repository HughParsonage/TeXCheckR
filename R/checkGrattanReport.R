#' Check Grattan Report
#'
#' @param path Path to search for the filename.
#' @param output_method How errors should be reported.
#' @return Called for its side-effect.
#' @export
#' @importFrom magrittr %>%
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom clisymbols symbol
#' @importFrom crayon green red bgGreen bgRed

checkGrattanReport <- function(path = ".",
                               output_method = c("console", "twitter")){
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))
  output_method <- match.arg(output_method)

  tex_file <- dir(path = ".", pattern = "\\.tex$")
  if (length(tex_file) != 1L){
    stop("path must contain one and only one .tex file.")
  }
  filename <- tex_file[[1]]

  the_authors <-
    get_authors(filename)

  cat("I see the following as authors:",
      the_authors, sep = "\n")

  .report_error <- function(...){
    report2console(...)
  }

  if (output_method == "twitter"){
    authors_twitter_handles <-
      Grattan_staff %>%
      .[and(name %in% the_authors,
            nchar(twitter_handle) > 0)] %>%
      .[["twitter_handle"]] %>%
      paste0("@", .)

    report_name <- gsub("^(.*)\\.tex$", "\\1", tex_file)

    .report_error <- function(...){
      report2twitter(...,
                     authors = authors_twitter_handles,
                     build_status = "Broken:",
                     report_name = report_name)
    }
  }

  check_cite_pagerefs(filename, .report_error = .report_error)
  cat("\n",
      green(symbol$tick, "Cite and pagerefs checked.\n"), sep = "")

  check_dashes(filename)
  cat(green(symbol$tick, "Dashes correctly typed.\n"))

  suppressMessages(check_footnote_typography(filename))
  cat(green(symbol$tick, "Footnote typography checked.\n"))

  check_repetitive_xrefs(filename)
  cat(green(symbol$tick, "No repetitive xrefs.\n"))

  check_sentence_ending_periods(filename)
  cat(green(symbol$tick, "Sentence-ending periods ok.\n"))

  check_spelling(filename)
  cat(green(symbol$tick, "Spellcheck complete.\n"))

  # To check the bibliography
  bib_files <-
    readLines(filename, warn = FALSE) %>%
    .[grepl("\\addbibresource", ., fixed = TRUE)] %>%
    gsub("^\\\\addbibresource[{](.+\\.bib)[}]$", "\\1", .)
  for (bib_file in bib_files){
    validate_bibliography(file = bib_file)
  }

  cat(green(symbol$tick, "Bibliography validated.\n"))

  check_labels(filename)

  cat(green(symbol$tick, "Labels checked.\n"))

  cat(bgGreen(symbol$tick, symbol$tick, "Report checked.\n"))

}
