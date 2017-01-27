#' Check cross-references
#' @description Check cross-references that are repetitive or incorrect case.
#' @param filename A LaTeX file
#' @param .report_error The function to provide context to the error.
#' @return NULL

check_xrefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)
  
  if (any(grepl("\\\\[cv]ref\\{", lines, perl = TRUE, ignore.case = FALSE))){
    if (any(grepl("\\\\cref\\{", lines, perl = TRUE, ignore.case = FALSE))){
      line_no <- grep("\\\\cref\\{", lines, perl = TRUE, ignore.case = FALSE)[[1]]
      context <- lines[[line_no]]
      .report_error(line_no = line_no,
                    context = context,
                    error_message = "Lower-case \\Cref used. \\Vref and \\Cref are case-sensitive and the lower-case forms are not permitted.")
      stop("Lower-case \\Cref used. \\Vref and \\Cref are case-sensitive and the lower-case forms are not permitted.")
    } else {
      line_no <- grep("\\\\vref\\{", lines, perl = TRUE, ignore.case = FALSE)[[1]]
      context <- lines[[line_no]]
      .report_error(line_no = line_no,
                    context = context,
                    error_message = "Lower-case \\Cref used. \\Vref and \\Cref are case-sensitive and the lower-case forms are not permitted.")
      stop("Lower-case \\Vref used. \\Vref and \\Cref are case-sensitive and the lower-case forms are not permitted.")
    }
  }

  line_nos_with_VrefCref <- grep("[CVcv]ref", lines, perl = TRUE)

  for (line in line_nos_with_VrefCref){
    # 'See Figure \Vref{fig:'
    if (grepl("((figure)|(table)|(box)) .[CVcv]ref", lines[[line]], perl = TRUE, ignore.case = TRUE)){
      .report_error(line_no = line,
                    context = lines[[line]],
                    error_message = "Repeated xref")
      stop("Repeated xref. May appear as e.g. 'See Figure Figure 4.2.'")
    }
  }
}
