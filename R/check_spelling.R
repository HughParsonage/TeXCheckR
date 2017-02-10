#' Spellchecker for Grattan reports
#'
#' @param filename Path to a LaTeX file to check.
#' @param pre_release Should the document be assumed to be final? Setting to \code{FALSE} allows function contents to be excluded.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of patterns known to be correct (which will never be raised by this function).
#' @param known.wrong Character vector of patterns known to be wrong.
#' @param bib_files Bibliography files (containing possible clues to misspellings).
#' @param .report_error A function to provide context to any errors.
#' @return If the spell check fails, the line at which the first error was detected, with an error message. If the check suceeds, \code{NULL} invisibly.
#' @details Uses the \code{en_AU} hunspell dictionary.
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @import hunspell
#' @export

check_spelling <- function(filename,
                           pre_release = TRUE,
                           ignore.lines = NULL,
                           known.correct = NULL,
                           known.wrong = NULL,
                           bib_files,
                           .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }

  file_path <- dirname(filename)
  lines <-
    read_lines(filename)

  if (any(grepl("\\documentclass", lines, fixed = TRUE))){
    lines <- gsub("{grattan}", "{report}", lines, fixed = TRUE)
  }

  if (!is.null(ignore.lines)){
    lines[ignore.lines] <- ""
  }

  # Never check URLS
  lines <- replace_nth_LaTeX_argument(lines, command_name = "url", replacement = "url")

  if (any(grepl("\\b(?:(?<!(\\\\))(?:(?:etc)|(?:i\\.?e)|(?:e\\.?g)))\\b", strip_comments(lines), perl = TRUE))){
    line_no <- grep("\\b(?:(?<!(\\\\))(?:(?:etc)|(?:i\\.?e)|(?:e\\.?g)))\\b", strip_comments(lines), perl = TRUE)[[1]]
    .report_error(error_message = "Use the macros \\etc, \\ie, and \\eg provided for consistent formatting.",
                  line_no = line_no,
                  context = lines[[line_no]])
    stop("Use the commands \\ie \\eg and \\etc rather than hard-coding.")
  }

  if (any(grepl("^[%] stop_if_present[:]", lines, perl = TRUE))){
    extra_known_wrong <-
      lines[grepl("^[%] stop_if_present[:]", lines, perl = TRUE)] %>%
      gsub("% stop_if_present: ", "", ., fixed = TRUE) %>%
      trimws %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist

    known.wrong <- c(known.wrong, extra_known_wrong)
  }

  # Do not check the bibliography filename
  lines <- gsub("\\{.*\\.bib\\}",
                "\\{bibliography.bib\\}",
                lines)

  if (any(grepl("\\begin{document}", lines, fixed = TRUE))){
    document_starts_at <- grep("\\begin{document}", lines, fixed = TRUE)
    lines_after_begin_document <- lines[-c(1:document_starts_at)]
  } else {
    document_starts_at <- 1
    lines_after_begin_document <- lines
  }

  if (AND(pre_release,
          any(grepl("% add_to_dictionary:", lines_after_begin_document, fixed = TRUE)))){
    .report_error(error_message = "When pre_release = TRUE, % add_to_dictionary: lines must not be situated outside the document preamble.")
    stop("When pre_release = TRUE, % add_to_dictionary: lines must not be situated outside the document preamble.")
  }

  words_to_add <- NULL
  if (any(grepl("% add_to_dictionary:", lines, fixed = TRUE))){
    words_to_add <-
      lines[grepl("% add_to_dictionary: ", lines, fixed = TRUE)] %>%
      gsub("% add_to_dictionary: ", "", ., fixed = TRUE) %>%
      trimws %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist

    known.correct <- c(known.correct, words_to_add)
  }






  # Check known wrong
  for (wrong in known.wrong){
    if (any(grepl(wrong, lines_after_begin_document, perl = TRUE))){
      line_no <- grep(wrong, lines_after_begin_document, perl = TRUE)[[1]]
      context <- lines_after_begin_document[[line_no]]
      .report_error(line_no = line_no + document_starts_at,
                    context = context,
                    error_message = paste0("'", wrong, "' present but prohibited in preamble."))
      stop(paste0("'", wrong, "' present but prohibited in preamble."))
    }
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
                       pre_release = pre_release,
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

  lines <- remove_valid_contractions(lines)

  # Ignore phantoms
  lines <- replace_LaTeX_argument(lines, command_name = "phantom", replacement = "PHANTOM")
  lines <- replace_LaTeX_argument(lines, command_name = "gls", replacement = "ENTRY")
  lines <- replace_LaTeX_argument(lines, command_name = "href", replacement = "correct")
  # Replace label argument in smallbox etc
  lines <- replace_nth_LaTeX_argument(lines,
                                      command_name = "begin.(?:(?:(?:very)?small)|(?:big))box[*]?[}]",
                                      n = 2L,
                                      replacement = "box:key")

  ignore_spelling_in_line_no <-
    grep("^[%] ignore.spelling.in: ", lines, perl = TRUE)

  if (pre_release && not_length0(ignore_spelling_in_line_no)){
    line_no <- ignore_spelling_in_line_no[1]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "pre_release = TRUE but 'ignore spelling in' line is present.")
    stop("pre_release = TRUE but 'ignore spelling in' line was present.")
  }

  if (!pre_release){
    commands_to_ignore <-
      lines[grepl("% ignore.spelling.in: ", lines, perl = TRUE)] %>%
      gsub("% ignore.spelling.in: ", "", ., perl = TRUE) %>%
      trimws %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist

    for (command in commands_to_ignore){
      lines <- replace_nth_LaTeX_argument(lines, command_name = command, replacement = "ignored")
    }
  }

  # Now we can strip comments as all the directives have been used
  lines <- strip_comments(lines)

  # Treat square brackets as invisible:
  # e.g. 'urgently phas[e] out' is correct
  # Need to avoid optional arguments to commands: use the spaces?
  lines <- rm_editorial_square_brackets(lines)

  lc_govt_pattern <-
    paste0("(?:",
           paste0("(?:Federal)",
                  "|",
                  "(?:Commonwealth)",
                  "|",
                  "(?:N(?:ew )?S(?:outh )?W(?:ales)?)",
                  "|",
                  "(?:Vic(?:torian?)?)",
                  "|",
                  "(?:Q(?:ueens)?l(?:an)?d)",
                  "|",
                  "(?:S(?:outh )?A(?:ustralian?)?)",
                  "|",
                  "(?:W(?:estern )?A(?:ustralian?)?)",
                  "|",
                  "(?:N(?:orthern? )?T(?:erritory)?)",
                  "|",
                  "(?:A(?:ustralian )?|C(?:apital )?T(?:erritory)?)"),
           ") government",
           "(?!\\s(?:schools?))")

  if (any(grepl(lc_govt_pattern, lines, perl = TRUE))){
    line_no <- grep(lc_govt_pattern, lines, perl = TRUE)[[1]]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Should be upper case G in government.")
    stop("Wrong case: 'government' should start with uppercase G in this context.")
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

    if (wrongly_spelled_word == "percent"){
      context <- paste0(lines[first_wrong_line_no], "\n",
                        "Use 'per cent', not 'percent'.")
    } else {
      context <- lines[first_wrong_line_no]
    }

    .report_error(line_no = first_wrong_line_no,
                  context = lines[first_wrong_line_no],
                  preamble = NULL,
                  "\n",
                  "\t", wrongly_spelled_word)
    stop("Common spelling error detected.")
  }

  valid_abbreviations <- extract_validate_abbreviations(lines)
  
  words_to_add <- c(valid_abbreviations, paste0(valid_abbreviations, "s"), words_to_add)

  parsed <- hunspell(lines, format = "latex", dict = dictionary("en_GB"))
  all_bad_words <- unlist(parsed)

  all_bad_words<- setdiff(all_bad_words,
                          c(CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE,
                            correctly_spelled_words,
                            words_to_add,
                            known.correct))

  are_misspelt <- vapply(parsed, not_length0, logical(1))

  notes <- NULL
  if (any(are_misspelt)){
    good_words <- c(correctly_spelled_words)
    # Detect big words first
    good_words <- good_words[order(-nchar(good_words))]
    gwp <- sprintf("(?:\\b%s\\b)",
                   paste0(good_words,
                          collapse = "\\b)|(?:\\b"))

    GWP <- sprintf("(?:\\b%s\\b)",
                   paste0(c(CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE, words_to_add, known.correct),
                          collapse = "\\b)|(?:\\b"))
    # Consult the bibliography
    # if any proper nouns
    if (!pre_release && !missing(bib_files) && any(grepl("^[A-Z]", all_bad_words, perl = TRUE))){
      authors_in_bib <-
        lapply(bib_files, extract_authors_from_bib) %>%
        unlist

      authors_in_bib_and_doc <-
        intersect(grep("^[A-Z]", all_bad_words,
                       value = TRUE, perl = TRUE),
                  authors_in_bib)
    } else {
      authors_in_bib <- authors_in_bib_and_doc <- NULL
    }
    assign("authors_in_bib_and_doc", value = authors_in_bib_and_doc, pos = parent.frame())

    for (line_w_misspell in which(are_misspelt)){
      # If bad words %in% ... don't bother checking
      bad_words <- setdiff(parsed[[line_w_misspell]],
                           c(CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE, correctly_spelled_words, words_to_add, known.correct))

      if (!pre_release){
        if (!is.null(authors_in_bib_and_doc)){
          bad_words_no_proper_nouns <- setdiff(bad_words, authors_in_bib)
          bad_words <- bad_words_no_proper_nouns
        }
      }

      if (not_length0(bad_words)){
        bad_line <- lines[[line_w_misspell]]
        # For timing
        bad_line_corrected <- gsub(gwp,
                                   "",
                                   bad_line,
                                   perl = TRUE,
                                   ignore.case = TRUE)
        bad_line_corrected <- gsub(GWP,
                                   "",
                                   bad_line_corrected,
                                   perl = TRUE,
                                   ignore.case = FALSE)
        recheck <- hunspell(bad_line_corrected,
                            format = "latex",
                            dict = dictionary("en_GB"))

        if (not_length0(recheck[[1]])){
          bad_word <- bad_words[[1]]
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
                        error_message = paste0("Spellcheck failed: '", bad_word, "'"),
                        extra_cat_post = c("\n",
                                           rep(" ", chars_b4_badword + 5 + nchar(line_w_misspell)),
                                           rep("^", nchar_of_badword),
                                           "\n"))
          stop("Spellcheck failed on above line with '", bad_word, "'")
        }
      }
    }
  }

  # Forgotten full stop.
  if (any(grepl("[a-z]\\.[A-Z]", lines, perl = TRUE))){
    line_no <- grep("[a-z]\\.[A-Z]", lines, perl = TRUE)[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Missing space between sentence. Likely reason: forgotten space.")
    stop("Missing space between sentence. Lower-case letter followed by full stop followed by capital letter.")
  }

  return(invisible(NULL))
}
