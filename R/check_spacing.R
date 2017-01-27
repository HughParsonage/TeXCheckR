
check_spacing <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)
  
  lines <- strip_comments(lines)
  
  lines_with_abbrev <- union(grep("\\\\etc\\s", lines, perl = TRUE),
                             union(grep("\\\\ie\\s", lines, perl = TRUE),
                                   grep("\\\\eg\\s", lines, perl = TRUE)))

  if (not_length0(lines_with_abbrev)){
    line_no <- sort(lines_with_abbrev)[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Command must not have whitespace after.")
    stop("Command ends with whitespace. Use the \\ie{} \\eg{} \\etc{} forms.")
  }
  invisible(NULL)
  
}
