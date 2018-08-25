#' Spell checking
#'
#' @param filename Path to a LaTeX file to check.
#' @param pre_release Should the document be assumed to be final?
#' Setting to \code{FALSE} permits the use of \code{ignore_spelling_in} and permits \code{add_to_dictionary} to be
#' present outside the document preamble.
#' @param ignore.lines Integer vector of lines to ignore (due to possibly spurious errors).
#' @param known.correct Character vector of patterns known to be correct (which will never be raised by this function).
#' @param known.correct.fixed Character vector of words known to be correct (which will never be raised by this function).
#' @param known.wrong Character vector of patterns known to be wrong.
#' @param ignore_spelling_in Command whose first mandatory argument will be ignored.
#' @param ignore_spelling_in_nth Named list of arguments to ignore; names are the commands to be ignored, values are the \code{n}th argument to be ignored.
#' @param bib_files Bibliography files (containing possible clues to misspellings). If supplied, and this function would otherwise throw an error, the \code{.bib} files are read and any author names that match the misspelled words are added to the dictionary.
#' @param check_etcs If \code{TRUE}, stop if any variations of \code{etc}, \code{ie}, and \code{eg} are present. (If they are typed literally, they may be formatted inconsistently. Using a macro ensures they appear consistently.)
#' @param dict_lang Passed to \code{hunspell::dictionary}.
#' @param rstudio Use the RStudio API?
#' @param .report_error A function to provide context to any errors. If missing, defaults to \code{\link{report2console}}.
#' @return Called primarily for its side-effect. If the spell check fails, the line at which the first error was detected, with an error message. If the check succeeds, \code{NULL} invisibly.
#' 
#' @details Extends and enhances \code{hunspell}:
#' 
#'  \itemize{
#' \item{You can add directives 
#' in the document itself. To add a word \code{foobaz} to the dictionary (so its presence does not throw an error), write
#' \code{\% add_to_dictionary: foobaz} on a single line. The advantage of this method is that you can collaborate
#' on the document without having to keep track of which spelling errors are genuine.}
#' \item{The 
#' directive \code{\% ignore_spelling_in: mycmd} which will ignore the spelling of words within the first argument
#' of \code{\\mycmd}.}
#' \item{\code{ignore_spelling_in_file: <file.tex>} will skip the check of \code{<file.tex>} if it is \code{input} or \code{include} in \code{filename}, as well as any files within it. Should appear as it is within \code{input} but with the file extension}
#' 
#' \item{Only the root document need be supplied; 
#' any files that are fed via \code{\\input} or \code{\\include} are checked (recursively).}
#' 
#' \item{A historical advantages was that the contents of certain commands were not checked, the spelling of which need not be checked 
#' as they are not printed, \code{viz.} citation and cross-reference commands, and certain optional arguments. Most of these
#' are now parsed correctly by \code{\link[hunspell]{hunspell}}, though some still need to be supplied (including, naturally, user-supplied macros).}
#' \item{Abbreviations and initialisms which are validly introduced will not throw errors. See \code{\link{extract_valid_abbrevations}}.}
#' \item{Words preceded by '[sic]' will not throw errors.}
#' }
#' 
#' The package comes with a suite of \code{\link{correctly_spelled_words}} that were not present in \code{hunspell}'s 
#' dictionary. 
#' 
#' This function should be quite fast, but slower than \code{hunspell::hunspell} (which it invokes). 
#' I aim for less than 500 ms on a real-world report of around 100 pages.
#' The function is slower when it needs to consult \code{bib_files}, though I recommend adding authors, titles, etc. 
#' to the dictionary
#' explicitly, or using \code{citeauthor} and friends. 
#' 
#' This function is forked from \url{https://github.com/hughparsonage/grattanReporter} to parse reports of the Grattan Institute, Melbourne for errors. See
#' \url{https://github.com/HughParsonage/grattex/blob/master/doc/grattexDocumentation.pdf} for the full spec.
#' Some checks that package performs have been omitted in this package.
#' 
#' @examples 
#' 
#' \dontrun{
#' url_bib <- 
#' paste0("https://raw.githubusercontent.com/HughParsonage/",
#'        "grattex/e6cab97145d38890e44e83d122e995e3b8936fc6/",
#'        "Report.tex")
#' check_spelling(url_bib)
#' }
#' 
#' @export
#' 

check_spelling <- function(filename,
                           pre_release = TRUE,
                           ignore.lines = NULL,
                           known.correct = NULL,
                           known.correct.fixed = NULL,
                           known.wrong = NULL,
                           ignore_spelling_in = NULL,
                           ignore_spelling_in_nth = NULL,
                           bib_files,
                           check_etcs = TRUE,
                           dict_lang = "en_GB",
                           rstudio = FALSE,
                           
                           .report_error){
  if (missing(.report_error)){
    if (rstudio) {
      if (!interactive()) {
        stop("Argument 'rstudio' is only valid in interactive sessions.")
      }
      if (!rstudioapi::verifyAvailable()) {
        stop("`rstudio = TRUE` yet RStudio is not running. ",
             "Either make sure you are using RStudio or select `rstudio = FALSE`.")
      }
      .report_error <- function(...) report2console(file = filename, ..., rstudio = TRUE)
    } else {
      .report_error <- function(...) report2console(...)
    }
  }

  file_path <- dirname(filename)
  orig <- lines <- read_lines(filename)
  
  # Quick way to identify end document if it exists, avoids issues 
  # with comments beneath \end{document}
  if (any(is_end_doc <- startsWith(lines, "\\end{document}"))) {
    lines <- lines[seq_len(which.max(is_end_doc))]
  }
  
  # Omits
  lines <- veto_sic(lines)
  # Smart quotes
  lines <- gsub(parse(text = paste0("'", "\u2019", "'")), "'", lines, fixed = TRUE)
  lines <- gsub(parse(text = paste0("'", "\u2018", "'")), "'", lines, fixed = TRUE)

  if (!is.null(ignore.lines)){
    lines[ignore.lines] <- ""
  }
  
  is_tikz <-
    cumsum(grepl("\\begin{tikzpicture}", lines, fixed = TRUE)) - 
    cumsum(grepl("\\end{tikzpicture}", lines, fixed = TRUE))
  
  lines[as.logical(is_tikz)] <- ""
  
  is_tikz <-
    cumsum(grepl("\\begin{align*}", lines, fixed = TRUE)) - 
    cumsum(grepl("\\end{align*}", lines, fixed = TRUE))
  
  lines[as.logical(is_tikz)] <- ""

  # Never check URLS
  lines <- replace_nth_LaTeX_argument(lines, command_name = "url", replacement = "url")
  
  # TODO: make this more general
  # Avoid getting etcs in lines
  lines <- gsub("-etc-", "-ETC-", lines, fixed = TRUE)
  
  etcs_pattern <- "\\b(?:(?<!(\\\\))(?:(?:etc)|(?:i\\.?e)|(?:e\\.?g)))\\b"
  if (check_etcs && any(grepl(etcs_pattern, strip_comments(lines), perl = TRUE))){
    line_no <- grep(etcs_pattern, strip_comments(lines), perl = TRUE)[[1]]
    column <- nchar(sub(paste0(etcs_pattern, ".*$"), "", lines[line_no], perl = TRUE))
    
    .report_error(error_message = "Use the macros \\etc, \\ie, and \\eg for consistent formatting.",
                  line_no = line_no,
                  column = column,
                  context = lines[[line_no]])
    stop("Use the commands \\ie \\eg and \\etc rather than hard-coding.")
  }

  if (any(grepl("^[%] stop_if_present[:]", lines, perl = TRUE))){
    extra_known_wrong <-
      lines[grepl("^[%] stop_if_present[:]", lines, perl = TRUE)] %>%
      gsub("% stop_if_present: ", "", ., fixed = TRUE) %>%
      stri_trim_both %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist

    known.wrong <- c(known.wrong, extra_known_wrong)
  }

  # Do not check the bibliography filename
  lines <- gsub("\\{.*\\.bib\\}",
                "\\{bibliography.bib\\}",
                lines)
  # e.g. printbibliography[prenote=customnote]
  lines[grepl("\\\\printbibliography", lines, perl = TRUE)] <- ""

  if (any(grepl("\\begin{document}", lines, fixed = TRUE))){
    document_starts_at <- grep("\\begin{document}", lines, fixed = TRUE)
    lines_after_begin_document <- lines[-c(1:document_starts_at)]
  } else {
    document_starts_at <- 1
    lines_after_begin_document <- lines
  }

  if (AND(pre_release,
          any(grepl("% add_to_dictionary:", lines_after_begin_document, fixed = TRUE)))) {
    first_line_no <- which.max(grepl("% add_to_dictionary:", lines_after_begin_document))
    .report_error(error_message = paste0("When pre_release = TRUE, ",
                                         "% add_to_dictionary: lines ", 
                                         "must not be situated outside the document preamble."),
                  line_no = first_line_no,
                  column = 1L)
    stop("When pre_release = TRUE, % add_to_dictionary: lines must not be situated outside the document preamble.")
  }

  words_to_add <- NULL
  if (any(grepl("% add_to_dictionary:", lines, fixed = TRUE))){
    words_to_add <-
      lines[startsWith(lines, "% add_to_dictionary: ")] %>%
      sub("% add_to_dictionary: ", "", ., fixed = TRUE) %>%
      stri_trim_both %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist %>%
      gsub("\\s", " ", ., fixed = TRUE)

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
  inputs <- inputs_of(filename)
  
  files_ignore <- 
    if (any(startsWith(lines, "% ignore_spelling_in_file:"))) {
      lines[startsWith(lines, "% ignore_spelling_in_file:")] %>%
        sub("^[%] ignore_spelling_in_file[:] (.*)([:][0-9]+)?\\s*$", "\\1", x = ., perl = TRUE)
    }

  if (!is.null(files_ignore)) {
    inputs %<>% setdiff(files_ignore)
  }
  
    
  
  commands_to_ignore <-
    if (!pre_release) {
      lines[grepl("% ignore.spelling.in: ", lines, perl = TRUE)] %>%
        gsub("% ignore.spelling.in: ", "", ., perl = TRUE) %>%
        stri_trim_both %>%
        strsplit(split = " ", fixed = TRUE) %>%
        unlist(use.names = FALSE)
    }

  if (length(inputs)) {
    # Recursively check
    cat_("Check subfiles:\n")
    for (input in inputs) {
      cat_(input, "\n")
      check_spelling(filename = file.path(file_path,
                                          paste0(sub("\\.tex?", "", input, perl = TRUE),
                                                 ".tex")),
                     pre_release = pre_release,
                     known.correct = known.correct,
                     known.wrong = known.wrong, 
                     ignore_spelling_in = c(commands_to_ignore, ignore_spelling_in),
                     dict_lang = dict_lang,
                     rstudio = rstudio)
    }
  }
  
  if (any(grepl("\\verb", lines, fixed = TRUE))) {
    lines <- gsub("\\\\verb(.)(.+?)\\1", "\\verb", lines)
  }
  
  # Do not check cite keys: not reliably supported by hunspell
  lines <-
    gsub(paste0("((foot)|(text)|(auto))",
                "cites?",
                # optional pre/postnote
                "((",
                # prenote
                "\\[", "[^\\]]*", "\\]",
                # postnote
                "(?:",
                "\\[", "[^\\]]*", "\\]",
                ")?",
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
  for (table_env in c("{tabularx}", "{tabular}", "{table}")) {
    lines <-
      if_else(grepl(paste0("\\begin", table_env), lines, fixed = TRUE),
              "\\begin{table-env}",
              lines)
  }

  

  # Valid ordinal patterns are permitted
  lines <-
    gsub(ordinal_pattern,
         "",
         lines,
         perl = TRUE)

  lines <- remove_valid_contractions(lines)

  # Ignore phantoms
  lines <- replace_nth_LaTeX_argument(lines, command_name = "phantom", replacement = "PHANTOM")
  lines <- replace_nth_LaTeX_argument(lines, command_name = "gls", replacement = "   ")
  lines <- replace_nth_LaTeX_argument(lines, command_name = "href", replacement = "correct")
  lines <- replace_nth_LaTeX_argument(lines, command_name = "vpageref", replacement = "correct")
  # Replace label argument in smallbox etc
  lines <- replace_nth_LaTeX_argument(lines,
                                      command_name = "begin.(?:(?:(?:very)?small)|(?:big))box[*]?[}]",
                                      n = 2L,
                                      replacement = "box:key")
  lines <- replace_nth_LaTeX_argument(lines, command_name = "verysmallbox", optional = TRUE)
  lines <- replace_nth_LaTeX_argument(lines,
                                      command_name = "[CVcv]refrange",
                                      n = 2L,
                                      replacement = "second range key")
  
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
  
  # Now we can strip comments as all the directives have been used
  # and it must occur before any fill_nth_LaTeX_argument arguments
  lines <- strip_comments(lines)
  
  parsed_doc <- parse_tex(lines)
  for (command in c(ignore_spelling_in, commands_to_ignore,
                    "captionsetup")) {
    if (any(grepl(sprintf("\\%s{", command), lines, fixed = TRUE))) {
      parsed_doc <- fill_nth_LaTeX_argument(parsed_doc, 
                                            command, 
                                            return.text = FALSE)
    }
  }
  if (!is.null(ignore_spelling_in_nth)) {
    for (isin in seq_along(ignore_spelling_in_nth)) {
      command <- names(ignore_spelling_in_nth)[[isin]]
      if (length(grep(sprintf("\\%s{", command), lines, fixed = TRUE))) {
        ns <- ignore_spelling_in_nth[[isin]]
        stopifnot(is.character(command), is.integer(ns))
        parsed_doc <- fill_nth_LaTeX_argument(parsed_doc, 
                                              command, 
                                              n = ns,
                                              return.text = FALSE)
      }
    }
    
  }
  

  lines <- unparse(parsed_doc)

  # Treat square brackets as invisible:
  # e.g. 'urgently phas[e] out' is correct
  # Need to avoid optional arguments to commands: use the spaces?
  lines <- rm_editorial_square_brackets(lines)

  if (any(grepl(lc_govt_pattern, lines, perl = TRUE))) {
    line_no <- grep(lc_govt_pattern, lines, perl = TRUE)[[1L]]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Should be upper case G in government.")
    stop("Wrong case: 'government' should start with uppercase G in this context.")
  }

  # Only applicable to Grattan reports
  if (AND(any(grepl("\\\\documentclass.*\\{grattan\\}", lines[1:10], perl = TRUE)),
          any(grepl(sprintf("\\b(%s)\\b", wrongly_spelled_words), lines, perl = TRUE)))) {
    first_wrong_line_no <-
      grep(sprintf("\\b(%s)\\b", wrongly_spelled_words), lines, perl = TRUE) %>%
      .[1]

    wrongly_spelled_word <-
      gsub(paste0("^.*\\b(", wrongly_spelled_words, ")\\b.*$"),
           "\\1",
           lines[first_wrong_line_no],
           perl = TRUE)

   
    if (wrongly_spelled_word == "percent") {
      context <- paste0(lines[first_wrong_line_no], "\n",
                        "Use 'per cent', not 'percent'.")
    } else {
      context <- lines[first_wrong_line_no]
    }

    .report_error(line_no = first_wrong_line_no,
                  context = lines[first_wrong_line_no],
                  "\n",
                  "\t", wrongly_spelled_word)
    stop("Common spelling error detected.")
  }

  valid_abbreviations <- extract_validate_abbreviations(lines)
  
  words_to_add <- c(valid_abbreviations, paste0(valid_abbreviations, "s"), words_to_add)

  parsed <- hunspell(lines, format = "latex", dict = dictionary(dict_lang),
                     ignore = c(hunspell::en_stats, known.correct.fixed))
  all_bad_words <- unlist(parsed)

  # Faster than using hunspell(add_words = )
  dictionary_additions <- c(CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE,
                            correctly_spelled_words,
                            words_to_add,
                            known.correct)
  all_bad_words <- 
    all_bad_words[all_bad_words %notin% dictionary_additions] %>%
    unique

  are_misspelt <- vapply(parsed, not_length0, logical(1))

  notes <- NULL
  if (any(are_misspelt)) {
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
        intersect(grep("^[A-Z]",
                       all_bad_words,
                       value = TRUE,
                       perl = TRUE),
                  authors_in_bib)
      
      dictionary_additions <- c(dictionary_additions, authors_in_bib_and_doc)
    } else {
      authors_in_bib <- authors_in_bib_and_doc <- NULL
    }
    assign("authors_in_bib_and_doc", value = authors_in_bib_and_doc, pos = parent.frame())

    for (line_w_misspell in which(are_misspelt)) {
      # If bad words %in% ... don't bother checking
      if (any(parsed[[line_w_misspell]] %notin% dictionary_additions)) {
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
                            dict = dictionary(dict_lang),
                            # Must be here in cases where multiple
                            # wrong words are on the one line (gsub
                            # may only pick up the first).
                            ignore = dictionary_additions)

        if (length(recheck[[1L]])) {
          # We've discovered a likely misspelling.
          # No need for performance, code within this if-statement
          # is just to prepare the error message.
          bad_word <- recheck[[1L]][[1L]]
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
                        column = chars_b4_badword + nchar_of_badword + 1L,
                        context = context,
                        error_message = paste0("Spellcheck failed: '", bad_word, "'"),
                        extra_cat_post = c("\n",
                                           rep(" ", chars_b4_badword + 5 + nchar(line_w_misspell)),
                                           rep("^", nchar_of_badword),
                                           "\n"))
          # Use %in% to avoid named logical var.
          if (rstudio && Sys.info()['sysname'] %in% "Windows") {
            suggested <- hunspell::hunspell_suggest(words = bad_word)[[1]]
            # Use charToRaw to avoid a trailing newline.
            # The extra space will be discarded (for some reason).
            utils::writeClipboard(charToRaw(paste0(suggested[1], " ")))
            cat("\nSuggested:\t", suggested)
          }
          stop("Spellcheck failed on above line with '", bad_word, "'")
        }
      }
    }
  }

  # Forgotten full stop.
  if (any(grepl("[a-z]\\.[A-Z]", lines, perl = TRUE))) {
    line_no <- grep("[a-z]\\.[A-Z]", lines, perl = TRUE)[[1L]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Missing space between sentence. Likely reason: forgotten space.")
    stop("Missing space between sentence. Lower-case letter followed by full stop followed by capital letter.")
  }

  return(invisible(NULL))
}
