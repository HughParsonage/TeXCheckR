#' Check that citations are all using cites
#' @param filename TeX document
#' @param .report_error Function to report errors
#' @export check_literal_citations

check_literal_citations <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  if (length(which_literal_citations(filename))){
    .report_error(line_no = which_literal_citations(filename)[[1]])
  }
}


which_literal_citations <- function(filename){
  lines <- 
    read_lines(filename) %>%
    strip_comments
  
  lines_with_et_al <-
    grep("\\bet al\\b", lines, perl = TRUE)
  
  lines_institutional_author_year <-
    grep("[A-Z]+\\s\\([12][0-9]{3}\\)", lines, perl = TRUE)
  
  author_pattern <-
    paste0("[A-Z][a-z]+",
           "(", 
           "( and ([A-Z][a-z]+))", 
           "|", 
           "(,? et al[.,]?)", 
           ")?", 
           "\\s", 
           "\\([12][0-9]{3}\\)")
  
  sort(union(union(lines_with_et_al, 
                   grep(author_pattern, lines, perl = TRUE)),
             lines_institutional_author_year))
}
