#' Check repetitive cross-references
#' @param filename A LaTeX file
#' @param .report_error The function to provide context to the error.
#' @return NULL

check_repetitive_xrefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)

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
