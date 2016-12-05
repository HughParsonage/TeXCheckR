#' Spellchecker for Grattan reports
#'
#' @param filename Path to a LaTeX file to check.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of words known to be correct (which will never be raised by this function).
#' @return If the spell check fails, the line at which the first error was detected, with an error message. If the check suceeds, \code{NULL} invisibly.
#' @details Uses the \code{en_AU} hunspell dictionary.
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary

check_spelling <- function(filename, ignore.lines = NULL, known.correct = NULL){
  lines <- readLines(filename, warn = FALSE)

  if (!is.null(ignore.lines)){
    lines <- lines[-ignore.lines]
  }

  if (!is.null(known.correct)){
    lines_corrected <- gsub(sprintf("\\b(%s)\\b", paste0(known.correct, collapse = ")|(")),
                            "correct",
                            lines,
                            perl = TRUE)
  } else {
    lines_corrected <- lines
  }

  parsed <- hunspell(lines, format = "latex", dict = dictionary("en_AU"))

  are_misspelt <- sapply(parsed, not_length0)

  if (any(are_misspelt)){
    cat(lines[are_misspelt][[1]])
    stop("Spellcheck failed on above line.")
  }
  invisible(NULL)
}
