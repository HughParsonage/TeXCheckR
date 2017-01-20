

check_escapes <- function(filename, .report_error){
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  
  if (any(grepl("(?<!(\\\\))[$]", lines, perl = TRUE))){
    line_no <- grep("(?<!(\\\\))[$]", lines, perl = TRUE)
    
    stop("Unescaped $. If math-mode is required, use \\( \\).")
  }
  invisible(NULL)
}