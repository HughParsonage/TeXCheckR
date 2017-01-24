#' Check dashes entered as hyphens
#'
#' @param filename A tex or Rnw file.
#' @param .report_error How errors should be reported.
#' @return File stops and \code{cat()}s on any line where a hyphen is surrounded by a space. 
#' Excludes dashes in knitr chunks and LaTeX math mode \code{\(...\)} but not in TeX math mode \code{$...$}.
#' @export

check_dashes <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  
  lines[isR_line_in_knitr(lines)] <- "%"
  
  lines <- strip_comments(lines)
  
  possible_hyphen <- grepl(" - ", lines, fixed = TRUE)
  
  if (any(possible_hyphen)){
    excluding_mathmode <- 
      if_else(possible_hyphen,
              gsub(paste0("\\\\[(]", "[^\\)]*", "\\\\[)]"),
                   "",
                   lines,
                   perl = TRUE),
              lines)
    
    if (any(grepl(" - ", excluding_mathmode, fixed = TRUE))){
      line_no <- grep(" - ", excluding_mathmode, fixed = TRUE)[[1]]
      .report_error(line_no = line_no,
                    context = lines[line_no],
                    error_message = "Dash likely masquerading as hyphen. Use -- for a dash.")
      stop("Dash likely masquerading as hyphen. Use -- for a dash.")
    }
    
  }
  invisible(NULL)
}
