

inputs_of <- function(filename, exclude.preamble = TRUE, append.tex = TRUE){
  file_path <- dirname(filename)
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
      rbindlist %>%
      setorderv("line_no_min") %>%
      .[["extract"]]
    
    if (append.tex) {
      out <- sprintf("%s.tex", tools::file_path_sans_ext(out))
    }
    
    return(out)
  } else {
    return(NULL)
  }
}
