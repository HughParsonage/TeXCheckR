#' Check that citations are all using cites
#' @param filename TeX document
#' @param .report_error Function to report errors
#' @export

check_literal_citations <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  lines <- 
    readLines(filename, encoding = "UTF-8", warn = FALSE) %>%
    strip_comments
  
  author_pattern <-
    paste0("[A-Z][a-z]+",
           "(", 
           "( and ([A-Z][a-z]+))", 
           "|", 
           "(,? et al\\.?)", 
           ")?", 
           "\\s", 
           "\\([12][0-9]{3}\\)")
  
  grep(author_pattern, lines, perl = TRUE)
}
