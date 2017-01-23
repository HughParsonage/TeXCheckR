#' Check quote marks in TeX
#' @description Checks whether a closing quote has been used at the start of a word.
#' @param filename LaTeX filename.
#' @param .report_error A function determining how errors will be reported.
#' @export

check_quote_marks <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  # Avoid ``ok''

  if (any(grepl("(^')|( ')", lines, perl = TRUE))){
    line_no <- grep("(^')|( ')", lines, perl = TRUE)[[1]]
    context <- lines[[line_no]]
    
    position <- gregexpr("(^')|( ')", context, perl = TRUE)[[1]][1] + nchar(line_no) + 5 # to match with X : etc.
    
    context <- paste0(substr(context, 0, position + 6), "\n", 
                      paste0(rep(" ", position - 1), collapse = ""), "^",
                      collapse = "")
    
    .report_error(line_no = line_no, 
                  context = context, 
                  error_message = "Closing quote used at beginning of word.")
    stop("Closing quote used at beginning of word.")
  }
  invisible(NULL)
}
