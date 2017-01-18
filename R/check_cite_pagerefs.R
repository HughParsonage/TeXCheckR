

check_cite_pagerefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- readLines(filename, warn = FALSE)

  line_nos_with_cites <-
    which(or(grepl("cite[", lines, fixed = TRUE),
             grepl("cite{", lines, fixed = TRUE)))

  for (line_no in line_nos_with_cites){
    line <- lines[[line_no]]
    if (grepl("cite\\[[^\\]]+\\][{]", line, perl = TRUE)){
      .report_error(line_no = line_no,
                    context = line,
                    error_message = "Use postnote for pagerefs.")
      stop("Use postnote for pagerefs.")
    }

    # Check constructions like p. 93
    if (grepl(paste0("cite\\[\\]",
                     "\\[pp?\\.?\\s*[0-9]+"),
              line,
              perl = TRUE)){
      message(line)
      stop("Unnecessary p in postnote.")
    }
    # Check single hyphen between pagerefs
    if (grepl("cite\\[\\]\\[[0-9]+-[0-9]+\\]", line, perl = TRUE)){
      message(line)
      stop("Page ranges should be separated by two hyphens (--).")
    }
  }

  invisible(NULL)
}
