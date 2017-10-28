#' Inputs to files nested within LaTeX document
#' @param filename The file whose \code{\\inputs} are to be extracted.
#' @param exclude.preamble (logical) If \code{TRUE}, the default, only \code{\\input}s and \code{\\include}s within the \code{document} environment are returned.
#' @param append.tex Should the result include the file extension \code{.tex}? By default, \code{TRUE}. Setting to \code{FALSE} may be useful when the file is not a \code{.tex} file.
#' @return A character vector of file paths relative to \code{filename} that are
#'  used as \code{\\input}s or \code{\\include}s within \code{filename}. If 
#'  no such files are present within \code{filename}, \code{NULL} is returned.
#' @export

inputs_of <- function(filename, exclude.preamble = TRUE, append.tex = TRUE) {
  lines <- read_lines(filename)
  
  if (any(grepl("\\end{document}", lines, fixed = TRUE))) {
    lines <- 
      lines[seq_along(lines) < grep("\\end{document}", lines, fixed = TRUE)]
  }
  
  lines_after_begin_document <-
    if (exclude.preamble && any(grepl("\\begin{document}", lines, fixed = TRUE))){
      lines[-seq_len(grep("\\begin{document}", lines, fixed = TRUE))]
    } else {
      lines
    }
  
  # inputs and includes
  inputs_in_doc <- length(grep("\\\\(?:(?:input)|(?:include(?!(graphics))))",
                               lines_after_begin_document,
                               perl = TRUE))
  
  if (inputs_in_doc > 0) {
    lines_with_possible_inputs <-
      grep("\\\\(?:(?:input)|(?:include))",
           lines_after_begin_document,
           perl = TRUE,
           value = TRUE)
    
    out <-
      lapply(c("input", "include"), 
             extract_LaTeX_argument, 
             tex_lines = lines_with_possible_inputs) %>%
      rbindlist
    
      if (nrow(out) > 0) {
        out <- 
          out %>%
          setorderv("line_no_min") %>%
          .[["extract"]]
        
        if (append.tex) {
          out <- sprintf("%s.tex", tools::file_path_sans_ext(out))
        }
      } else {
        out <- NULL
      }
    
    return(out)
  } else {
    return(NULL)
  }
}
