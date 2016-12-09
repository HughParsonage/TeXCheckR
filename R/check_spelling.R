#' Spellchecker for Grattan reports
#'
#' @param filename Path to a LaTeX file to check.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of words known to be correct (which will never be raised by this function).
#' @return If the spell check fails, the line at which the first error was detected, with an error message. If the check suceeds, \code{NULL} invisibly.
#' @details Uses the \code{en_AU} hunspell dictionary.
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @export

check_spelling <- function(filename, ignore.lines = NULL, known.correct = NULL){
  lines <- readLines(filename, warn = FALSE)[-1]

  if (!is.null(ignore.lines)){
    lines <- lines[-ignore.lines]
  }

  # Do not check the bibliogrpahy filename
  lines <- gsub("\\{.*\\.bib\\}",
                "\\{bibliography.bib\\}",
                lines)

  # inputs and includes
  inputs_in_doc <- length(grep("\\\\(?:(?:input)|(?:include(?!(graphics))))", lines, perl = TRUE))

  inputs <- gsub("^\\\\(?:(?:input)|(?:include(?!(graphics))))[{](.*(?:\\.tex)?)[}]$",
                 "\\2",
                 lines[grepl("^\\\\(?:(?:input)|(?:include(?!(graphics))))[{](.*(\\.tex)?)[}]$", lines, perl = TRUE)],
                 perl = TRUE)

  stopifnot(length(inputs) == length(inputs_in_doc))

  # Do not check cite keys
  lines <- gsub(paste0("((foot)|(text)|(auto))",
                       "cites?",
                          # optional pre/postnote
                          "((",
                          # prenote
                          "\\[", "\\]",
                          # postnote
                          "\\[", "[^\\]]*", "\\]",
                          ")?",
                       # cite key (possibly multiple)
                       # (the multiplicity applies to the prenote as well)
                       "[{]", "[^\\}]+", "[}])+",

                       collapse = ""),
                "\\1cite\\{citation\\}",
                lines,
                perl = TRUE)

  # Do not check labels
  lines <- gsub(paste0("(",
                          "\\\\[VCvc]?",
                          "(ref)|(label)",
                       "\\{)",
                          "([^\\}]+)",
                       "\\}"),
                "\\1correct\\}",
                lines,
                perl = TRUE)

  # box labels
  lines <- gsub("(((small)|(big))box[*]?[}][{][^\\}]+[}][{])[^\\}]+[}]",
                "\\1box-key\\}",
                lines,
                perl = TRUE)

  # itemize enumerate optional arguments
  lines <- gsub("(\\\\begin[{](?:(?:itemize)|(?:enumerate))[}])(\\[[^\\]]+\\])?",
                "\\1",
                lines,
                perl = TRUE)

  # Just completely ignore tabularx lines
  lines <- if_else(grepl("\\begin{tabularx}", lines, fixed = TRUE),
                   "\\begin{tabularx}{\\linewidth}{XXXX}",
                   lines)

  # Treat square brackets as invisible:
  # e.g. 'urgently phas[e] out' is correct
  # Need to avoid optional arguments to commands: use the spaces?


  lines_corrected <- gsub(sprintf("\\b(%s)\\b", correctly_spelled_words),
                          "correct",
                          lines,
                          perl = TRUE)


  if (!is.null(known.correct)){
    # replace these words with the word 'correct'
    lines_corrected <- gsub(sprintf("(\\b%s\\b)", paste0(known.correct, collapse = "\\b)|(\\b")),
                            "correct",
                            lines_corrected,
                            perl = TRUE)
  }

  parsed <- hunspell(lines_corrected, format = "latex", dict = dictionary("en_AU"))

  are_misspelt <- sapply(parsed, not_length0)

  if (any(are_misspelt)){
    cat(lines[are_misspelt][[1]], "\n")
    cat("\t", unlist(hunspell(lines_corrected[are_misspelt][[1]], format = "latex", dict = dictionary("en_AU"))), "\n")
    stop("Spellcheck failed on above line.")
  }

  return(invisible(NULL))
}
