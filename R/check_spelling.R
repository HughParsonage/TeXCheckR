#' Spellchecker for Grattan reports
#'
#' @param filename Path to a LaTeX file to check.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of patterns known to be correct (which will never be raised by this function).
#' @param known.wrong Character vector of patterns known to be wrong.
#' @param .report_error A function to provide context to any errors.
#' @return If the spell check fails, the line at which the first error was detected, with an error message. If the check suceeds, \code{NULL} invisibly.
#' @details Uses the \code{en_AU} hunspell dictionary.
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @import hunspell
#' @export

check_spelling <- function(filename,
                           ignore.lines = NULL,
                           known.correct = NULL,
                           known.wrong = NULL,
                           .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }

  file_path <- dirname(filename)
  lines <-
    readLines(filename, warn = FALSE, encoding = "UTF-8")

  if (any(grepl("\\documentclass", lines, fixed = TRUE))){
    lines <- gsub("{grattan}", "{report}", lines, fixed = TRUE)
  }

  if (!is.null(ignore.lines)){
    lines[ignore.lines] <- ""
  }
  
  if (any(grepl("\\b(?:(?<!(\\\\))(?:(?:etc)|(?:ie)|(?:eg)))\\b", lines, perl = TRUE))){
    line_no <- grep("\\b(?:(?<!(\\\\))(?:(?:etc)|(?:ie)|(?:eg)))\\b", lines, perl = TRUE)[[1]]
    .report_error(error_message = "Use the macros \\etc, \\ie, and \\eg provided for consistent formatting.",
                  line_no = line_no,
                  context = lines[[line_no]])
    stop("Use the commands \\etc.")
  }

  # Check known wrong
  for (wrong in known.wrong){
    if (any(grepl(wrong, lines, perl = TRUE))){
      cat(grep(wrong, lines, perl = TRUE)[[1]])
      cat(wrong)
      stop("A pattern you have raised was detected in the document.")
    }
  }

  # Do not check the bibliography filename
  lines <- gsub("\\{.*\\.bib\\}",
                "\\{bibliography.bib\\}",
                lines)

  lines_after_begin_document <-
    if (any(grepl("\\begin{document}", lines, fixed = TRUE))){
      lines[-c(1:grep("\\begin{document}", lines, fixed = TRUE))]
    } else {
      lines
    }

  # inputs and includes
  inputs_in_doc <- length(grep("\\\\(?:(?:input)|(?:include(?!(graphics))))",
                               lines_after_begin_document,
                               perl = TRUE))

  if (inputs_in_doc > 0){
    inputs <- gsub("^\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{](.*(?:\\.tex)?)[}]$",
                   "\\1",
                   lines_after_begin_document[grepl("^\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{](.*(\\.tex)?)[}]$",
                                                    lines_after_begin_document,
                                                    perl = TRUE)],
                   perl = TRUE)

    if (length(inputs) != inputs_in_doc){
      stop("Unable to parse inputs. Check they are all of the form \\input{filename}.")
    }


    # Recursively check
    if (length(inputs) > 0){
      cat("Check subfiles:\n")
      for (input in inputs){
        cat(input, "\n")
        check_spelling(filename = file.path(file_path,
                                            paste0(input, ".tex")),
                       known.correct = known.correct,
                       known.wrong = known.wrong)
      }
    }
  }

  # Do not check cite keys
  lines <-
    gsub(paste0("((foot)|(text)|(auto))",
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
                       "\\\\(([VCvc]?(page)?)|(top)|([Cc]hap))?",
                       "(ref(range)?)|(label)",
                       ")",
                       "\\{",
                       "([^\\}]+)",
                       "\\}"),
                "\\1\\{correct\\}",
                lines,
                perl = TRUE)

  # box labels
  lines <-
    gsub(paste0("(",
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
  lines <-
    gsub("(\\\\begin[{](?:(?:itemize)|(?:enumerate))[}])(\\[[^\\]]+\\])?",
         "\\1",
         lines,
         perl = TRUE)

  # Just completely ignore tabularx tabular table lines
  for (table_env in c("{tabularx}", "{tabular}", "{table}")){
    lines <-
      if_else(grepl(paste0("\\begin", table_env), lines, fixed = TRUE),
              "\\begin{table-env}",
              lines)
  }

  # Ignore captionsetups
  lines <- replace_LaTeX_argument(lines, "captionsetup", "")

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
         "",
         lines,
         perl = TRUE)

  # Ignore phantoms
  lines <- replace_LaTeX_argument(lines, command_name = "phantom", replacement = "PHANTOM")
  lines <- replace_LaTeX_argument(lines, command_name = "gls", replacement = "ENTRY")
  lines <- replace_LaTeX_argument(lines, command_name = "href", replacement = "correct")
  # Replace label argument in smallbox etc
  lines <- replace_nth_LaTeX_argument(lines,
                                      command_name = "begin.(?:(?:(?:very)?small)|(?:big))box[*]?[}]",
                                      n = 2L,
                                      replacement = "box:key")

  # Treat square brackets as invisible:
  # e.g. 'urgently phas[e] out' is correct
  # Need to avoid optional arguments to commands: use the spaces?
  lines <- rm_editorial_square_brackets(lines)

  words_to_add <- NULL
  if (any(grepl("% add_to_dictionary:", lines, fixed = TRUE))){
    words_to_add <-
      lines[grepl("% add_to_dictionary: ", lines, fixed = TRUE)] %>%
      gsub("% add_to_dictionary: ", "", ., fixed = TRUE) %>%
      trimws %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist
  }

  if (any(grepl(sprintf("\\b(%s)\\b", wrongly_spelled_words), lines, perl = TRUE))){
    first_wrong_line_no <-
      grep(sprintf("\\b(%s)\\b", wrongly_spelled_words), lines, perl = TRUE) %>%
      .[1]

    wrongly_spelled_word <-
      gsub(paste0("^.*\\b(", wrongly_spelled_words, ")\\b.*$"),
           "\\1",
           lines[first_wrong_line_no],
           perl = TRUE)

    .report_error(line_no = first_wrong_line_no,
                  context = lines[first_wrong_line_no],
                  preamble = NULL,
                  "\n",
                  "\t", wrongly_spelled_word)
    stop("Common spelling error detected.")
  }

  parsed <- hunspell(lines, format = "latex", dict = dictionary("en_GB"))

  are_misspelt <- vapply(parsed, not_length0, logical(1))

  if (any(are_misspelt)){
    for (line_w_misspell in which(are_misspelt)){
      bad_words <- parsed[[line_w_misspell]]
      for (bad_word in bad_words){
        if (bad_word %notin% c(correctly_spelled_words, words_to_add, known.correct)){
          bad_line <- lines[[line_w_misspell]]
          bad_line_corrected <- bad_line
          for (good_word in c(correctly_spelled_words, words_to_add, known.correct)){
            bad_line_corrected <- gsub(paste0("\\b", good_word, "\\b"),
                                       "",
                                       bad_line_corrected,
                                       perl = TRUE,
                                       ignore.case = TRUE)
          }
          recheck <- hunspell(bad_line_corrected,
                              format = "latex",
                              dict = dictionary("en_GB"))

          if (not_length0(recheck[[1]])){
            nchar_of_badword <- nchar(bad_word)

            chars_b4_badword <-
              gsub(sprintf("^(.*)(?:%s).*$", bad_word),
                   "\\1",
                   lines[[line_w_misspell]],
                   perl = TRUE) %>%
              nchar

            context <-
              if (chars_b4_badword + nchar_of_badword < 80){
                substr(lines[[line_w_misspell]], 0, 80)
              } else {
                lines[[line_w_misspell]]
              }

            .report_error(line_no = line_w_misspell,
                          context = context,
                          error_message = paste0(c("Spellcheck failed:'", bad_word, "'",
                                                   collapse = NULL)),
                          extra_cat = c("\n",
                                        rep(" ", chars_b4_badword + 5 + nchar(line_w_misspell)),
                                        rep("^", nchar_of_badword),
                                        "\n"))
            stop("Spellcheck failed on above line with '", bad_word, "'")
          }
        }
      }
    }
  }

  return(invisible(NULL))
}
