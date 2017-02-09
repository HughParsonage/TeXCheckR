#' Check quote marks in TeX
#' @description Checks whether a closing quote has been used at the start of a word.
#' @param filename LaTeX filename.
#' @param .report_error A function determining how errors will be reported.
#' @export

check_quote_marks <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  lines <- 
    read_lines(filename) %>%
    strip_comments
  # Avoid ``ok''
  
  bad_open_quote_regex <- 
    sprintf("%s|%s|%s|%s|%s|%s|%s|%s|%s|%s", 
            "(^')",
            "( ')",
            "(\\(')",
            "(\\[')",
            "(-')",
            # Double quotes too:
            '(^")',
            '( ")',
            '(\\(")',
            '(\\[")',
            '(-")')

  if (any(grepl(bad_open_quote_regex, lines, perl = TRUE))){
    line_no <- grep(bad_open_quote_regex, lines, perl = TRUE)[[1]]
    context <- lines[[line_no]]
    
    if (grepl("^'", context, perl = TRUE)){
      position <- 5
    } else {
      position <- gregexpr(bad_open_quote_regex, context, perl = TRUE)[[1]][1] + nchar(line_no) + 6 # to match with X : etc.
    }
    context <- paste0(substr(context, 0, position + 6), if (nchar(context) > position + 6) "...\n" else "\n", 
                      paste0(rep(" ", position - 1), collapse = ""), "^", 
                      collapse = "")
    
    .report_error(line_no = line_no, 
                  context = context, 
                  error_message = "Closing quote used at beginning of word. Use a backtick for an opening quote, e.g. The word `ossifrage' is quoted.")
    stop("Closing quote used at beginning of word. Use a backtick for an opening quote, e.g. The word `ossifrage' is quoted.")
  }
  invisible(NULL)
}
