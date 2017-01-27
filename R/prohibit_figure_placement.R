

prohibit_figure_placement <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- read_lines(filename)
  
  if (any(grepl("\\begin{figure}[", lines, fixed = TRUE))){
    line_no <- grep("\\begin{figure}[", lines, fixed = TRUE)
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Manual placement of figures is not permitted.")
    stop("Modification of figure float parameters is not permitted. (Only movement.)")
  }
}
