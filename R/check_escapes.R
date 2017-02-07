#' Check escapes
#' @description Checks file for unescaped dollar signs. 
#' With these present, there is a risk of constructions like \code{We gave $10 to a million people at a cost of $10~million dollars.},
#' which is valid syntax, but incorrectly formatted. Accordingly, math-mode must be more assertively requested using \code{\(..\)}.
#' @param filename File in which to report the error
#' @param .report_error How the errors should be reported.
#' @return An error if unescaped dollar signs are present in \code{filename}. Otherwise, \code{NULL} invisibly.
#' @export

check_escapes <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)
  
  if (any(grepl("(?<!(\\\\))[$]", lines, perl = TRUE))){
    line_no <- grep("(?<!(\\\\))[$]", lines, perl = TRUE)[[1]]
    context <- lines[[line_no]]
    position <- gregexpr("(?<!(\\\\))[$]", context, perl = TRUE)[[1]][1] + nchar(line_no) + 5 # to match with X : etc.
    context <- paste0(substr(context, 0, position + 6), "\n", 
                      paste0(rep(" ", position - 1), collapse = ""), "^",
                      collapse = "")
    .report_error(line_no = line_no, 
                  context = context, 
                  error_message = "Unescaped $.",
                  advice = "If you meant to print a dollar sign, use \\$. If you want to use math-mode, use \\(...\\), not $...$ .")
    stop("Unescaped $. If you meant to print a dollar sign, use \\$. If you want to use math-mode, use \\(...\\), not $...$ .")
  }
  
  if (any(grepl("...", lines, fixed = TRUE)) || any(grepl("\u2026", lines, fixed = TRUE))){
    line_no <- which(grepl("...", lines, fixed = TRUE) | grepl("\u2026", lines, fixed = TRUE))[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Ellipsis typed without using macro.",
                  advice = "Use \\dots{} for an ellipsis, rather than three dots (...) or \\u2026 (\u2026).")
    stop("Use \\dots{} for an ellipsis, rather than three dots (...) or \\u2026 (\u2026).")
  }
  
  invisible(NULL)
}
