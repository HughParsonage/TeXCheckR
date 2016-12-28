#' Spellchecker for Grattan reports
#'
#' @param filename Path to a LaTeX file to check.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of patterns known to be correct (which will never be raised by this function).
#' @param known.wrong Character vector of patterns known to be wrong.
#' @return If the spell check fails, the line at which the first error was detected, with an error message. If the check suceeds, \code{NULL} invisibly.
#' @details Uses the \code{en_AU} hunspell dictionary.
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @import hunspell
#' @export

check_spelling <- function(filename, ignore.lines = NULL, known.correct = NULL, known.wrong = NULL){
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")[-1]

  if (!is.null(ignore.lines)){
    lines[ignore.lines] <- ""
  }

  # Check known wrong
  for (wrong in known.wrong){
    if (any(grepl(wrong, lines))){
      cat(wrong)
      stop("A pattern you have raised was detected in the document.")
    }
  }

  # Do not check the bibliogrpahy filename
  lines <- gsub("\\{.*\\.bib\\}",
                "\\{bibliography.bib\\}",
                lines)

  lines_after_begin_document <- lines[-c(1:grep("\\begin{document}", lines, fixed = TRUE))]

  # inputs and includes
  inputs_in_doc <- length(grep("\\\\(?:(?:input)|(?:include(?!(graphics))))", lines_after_begin_document, perl = TRUE))

  if (inputs_in_doc > 0){
    inputs <- gsub("^\\\\(?:(?:input)|(?:include(?!(graphics))))[{](.*(?:\\.tex)?)[}]$",
                   "\\2",
                   lines_after_begin_document[grepl("^\\\\(?:(?:input)|(?:include(?!(graphics))))[{](.*(\\.tex)?)[}]$", lines_after_begin_document, perl = TRUE)],
                   perl = TRUE)

    if (length(inputs) != inputs_in_doc){
      stop("Unable to parse inputs. Check they are all of the form \\input{filename}.")
    }


    # Recursively check
    if (length(inputs) > 0){
      for (input in inputs){
        tryCatch(check_spelling(filename = paste0(input, ".tex"), known.correct = known.correct, known.wrong = known.wrong),
                 # Display the filename as well as the error returned.
                 error = function(e){cat(input); e})
      }
    }
  }

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
                          "\\\\(([VCvc])|(top)|(chap))?",
                          "(ref)|(label)",
                       "\\{)",
                          "([^\\}]+)",
                       "\\}"),
                "\\1correct\\}",
                lines,
                perl = TRUE)

  # box labels
  lines <- gsub(paste0("(",
                       "((small)|(big))box[*]?",
                       "[}]",

                       # optional placement parameter
                       # e.g. \begin{smallbox}[!h]
                       "(",
                       "\\[",
                       # empty optional argument unlikely,
                       # but not for this function to check.
                       "[!htbpH]*",
                       "\\]",
                       ")?",
                       # title argument
                       "[{][^\\}]+[}]",
                       # key argument:
                       "[{]",
                       ")", # caputure everything except the actual key
                       "[^\\}]+[}]"),
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

  # Valid ordinal patterns are permitted
  ordinal_pattern <-
    paste0("((?<!1)1(\\\\textsuperscript\\{)?st)",
           "|",
           "((?<!1)2(\\\\textsuperscript\\{)?nd)",
           "|",
           "((?<!1)3(\\\\textsuperscript\\{)?rd)",
           "|",
           "(([04-9]|(1[1-3]))(\\\\textsuperscript\\{)?th)")
  stopifnot(identical(grepl(ordinal_pattern,
                            c("3rd", "11th", "21st", "13th", "13rd", "101st", "11st", "funding", "3\\textsuperscript{rd}"),
                            perl = TRUE),
                      c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)))

  lines <-
    gsub(ordinal_pattern,
         "correct",
         lines,
         perl = TRUE)

  # Ignore phantoms
  lines <- replace_LaTeX_argument(lines, command_name = "phantom", replacement = "correct")

  # Treat square brackets as invisible:
  # e.g. 'urgently phas[e] out' is correct
  # Need to avoid optional arguments to commands: use the spaces?
  lines <- rm_editorial_square_brackets(lines)

  lines_corrected <- gsub(sprintf("\\b(%s)\\b", correctly_spelled_words),
                          "correct",
                          lines,
                          perl = TRUE)

  if (any(grepl("% add_to_dictionary:", lines, fixed = TRUE))){
    words_to_add <-
      lines[grepl("% add_to_dictionary: ", lines, fixed = TRUE)] %>%
      gsub("% add_to_dictionary: ", "", ., fixed = TRUE) %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist

    known.correct <- union(known.correct, words_to_add)
  }


  if (!is.null(known.correct)){
    # replace these words with the word 'correct'
    lines_corrected <- gsub(sprintf("(\\b%s\\b)", paste0(known.correct, collapse = "\\b)|(\\b")),
                            "correct",
                            lines_corrected,
                            perl = TRUE)
  }



  parsed <- hunspell(lines_corrected, format = "latex", dict = dictionary("en_GB"))

  are_misspelt <- sapply(parsed, not_length0)

  if (any(are_misspelt)){
    cat(lines[are_misspelt][[1]], "\n")
    cat("\t", unlist(hunspell(lines_corrected[are_misspelt][[1]], format = "latex", dict = dictionary("en_GB"))), "\n")
    stop("Spellcheck failed on above line.")
  }

  return(invisible(NULL))
}
