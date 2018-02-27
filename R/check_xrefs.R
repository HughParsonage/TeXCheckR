#' Check cross-references
#' @description Check cross-references that are repetitive or (in the case of \verb{cleveref} and \verb{varioref}) incorrect case.
#' @param filename A LaTeX file
#' @param permitted.case One of \code{NA, "upper", "lower"}. If \code{NA}, the default,
#' both \verb{\Cref} and \verb{\cref} are permitted, but not in the same document.
#' If \code{upper}, only \verb{\Cref} 
#' is permitted; if \code{lower}, only \verb{\cref}.
#' If \code{NULL}, the case is not checked at all.
#' @param .report_error The function to provide context to the error.
#' @return NULL

check_xrefs <- function(filename, permitted.case = c(NA, "upper", "lower"), .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)
  
  if (!is.null(permitted.case)) {
    case <- match.arg(permitted.case)
    if (is.na(case)) {
      if (any(grepl("\\Cref{", lines, fixed = TRUE)) || 
          any(grepl("\\Vref{", lines, fixed = TRUE))) {
        case <- "upper"
      } else {
        case <- "lower"
      }
    }
    
    if (case == "upper") {
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
                        error_message = "Lower-case \\Cref used. \\Vref and \\Cref are case-sensitive and the lower-case forms are not permitted.",
                        halt = TRUE)
        }
      }
    } else if (case == "lower") {
      if (any(grepl("\\\\[CV]ref\\{", lines, perl = TRUE, ignore.case = FALSE))){
        if (any(grepl("\\\\Cref\\{", lines, perl = TRUE, ignore.case = FALSE))){
          line_no <- grep("\\\\Cref\\{", lines, perl = TRUE, ignore.case = FALSE)[[1]]
          context <- lines[[line_no]]
          .report_error(line_no = line_no,
                        context = context,
                        error_message = "Upper-case \\cref used. \\vref and \\cref are case-sensitive and the upper-case forms are not permitted.",
                        halt = TRUE)
        } else {
          line_no <- grep("\\\\Vref\\{", lines, perl = TRUE, ignore.case = FALSE)[[1]]
          context <- lines[[line_no]]
          .report_error(line_no = line_no,
                        context = context,
                        error_message = "Upper-case \\vref used. \\vref and \\cref are case-sensitive and the lower-case forms are not permitted.", 
                        halt = TRUE)
        }
      }
    } else {stop("Unexpected error: check_xref 37.")}
  }
    
  line_nos_with_VrefCref <- grep("[CVcv]ref", lines, perl = TRUE)
  
  for (line in line_nos_with_VrefCref) {
    # 'See Figure \Vref{fig:'
    if (grepl("((figure)|(table)|(box)) .[CVcv]ref", lines[[line]], perl = TRUE, ignore.case = TRUE)){
      .report_error(line_no = line,
                    context = lines[[line]],
                    error_message = "Repeated xref")
      stop("Repeated xref. May appear as e.g. 'See Figure Figure 4.2.'")
    }
  }
}
