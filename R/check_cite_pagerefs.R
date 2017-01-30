

check_cite_pagerefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)

  line_nos_with_cites <-
    grep("cites?[\\[\\{]", lines, perl = TRUE)

  for (line_no in line_nos_with_cites){
    line <- lines[[line_no]]
    if (grepl("cites?\\[[^\\]]+\\][{]", line, perl = TRUE)){
      .report_error(line_no = line_no,
                    context = line,
                    error_message = "Use postnote for pagerefs.")
      stop("Use postnote for pagerefs.")
    }

    # Check constructions like p. 93
    if (grepl(paste0("cites?\\[\\]",
                     "\\[pp?\\.?\\s*[0-9]+"),
              line,
              perl = TRUE)){
      message(line)
      stop("Unnecessary p in postnote.")
    }
    # Check single hyphen between pagerefs
    if (grepl("cites?\\[\\]\\[[0-9]+-[0-9]+\\]", line, perl = TRUE)){
      message(line)
      stop("Page ranges should be separated by two hyphens (--).")
    }
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
    line_no <- line_no[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = lines[[line_no]],
                  error_message = paste0("Use of singular form of textcite or footcite.")
                  )
    stop("Use of singular form of \\textcite or \\footcite but { follows immediately after the first key. ",
         "Did you mean \\textcites or \\footcites ?")
  }


  invisible(NULL)
}
