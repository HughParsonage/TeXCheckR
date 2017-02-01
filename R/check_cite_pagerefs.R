

check_cite_pagerefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)

  line_nos_with_cites <-
    grep("cites?[\\[\\{]", lines, perl = TRUE)

  if (any(grepl("cites?\\[[^\\]]+\\][{]", lines[line_nos_with_cites], perl = TRUE))){
    line_no <- line_nos_with_cites[grepl("cites?\\[[^\\]]+\\][{]", lines[line_nos_with_cites], perl = TRUE)][[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = line,
                  error_message = "Use postnote for pagerefs.")
    stop("Use postnote for pagerefs.")
  }

    # Check constructions like p. 93
  if (any(grepl(r2("cites?\\[\\]",
                   "\\[pp?\\.?\\s*[0-9]+"),
                lines[line_nos_with_cites],
                perl = TRUE))){
    line_no <- line_nos_with_cites[grepl(r2("cites?\\[\\]",
                                           "\\[pp?\\.?\\s*[0-9]+"),
                                        lines[line_nos_with_cites],
                                        perl = TRUE)][[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = line,
                  error_message = "Unnecessary p in postnote.")
    stop("Unnecessary p in postnote.")
  }
  # Check single hyphen between pagerefs
  if (any(grepl("cites?\\[\\]\\[[0-9]+-[0-9]+\\]", lines[line_nos_with_cites], perl = TRUE))){
    line_no <- line_nos_with_cites[grepl("cites?\\[\\]\\[[0-9]+-[0-9]+\\]",
                                         lines[line_nos_with_cites],
                                         perl = TRUE)][[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = line,
                  error_message = "Page ranges should be separated by two hyphens (--).")
    stop("Page ranges should be separated by two hyphens (--).")
  }

  # Check footcites when only one key:

  plural_cite_but_single_key <-
    paste0("((?:text)|(?:foot))cite",
           # optional pre/postnote
           r5("(?:",
              "\\[\\]\\[",
              r3("[", "^\\]", "]+"),
              "\\]",
              ")?"),
           # This key
           "\\{[^\\}]+\\}",
           # Offender: if present, should be cites
           "\\{")

  if (any(grepl(plural_cite_but_single_key,
                lines[line_nos_with_cites],
                perl = TRUE))){
    line_nos <- grep(plural_cite_but_single_key,
                    lines,
                    perl = TRUE)
    line_no <- line_nos[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = lines[[line_no]],
                  error_message = paste0("Use of singular form of textcite or footcite."))
    stop("Use of singular form of \\textcite or \\footcite but { follows immediately after the first key. ",
         "Did you mean \\textcites or \\footcites ?")
  }

  invisible(NULL)
}
