

inputs_of <- function(filename, append.tex = TRUE){
  file_path <- dirname(filename)
  lines <- read_lines(filename)
  
  lines_after_begin_document <-
    if (any(grepl("\\begin{document}", lines, fixed = TRUE))){
      lines[-c(1:grep("\\begin{document}", lines, fixed = TRUE))]
    } else {
      lines
    }
  
  # inputs and includes
  inputs_in_doc <- length(grep("\\\\(?:(?:input)|(?:include(?!(graphics))))",
                               lines_after_begin_document,
                               perl = TRUE))
  
  if (inputs_in_doc > 0){
    inputs <- gsub("^\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{](.*(?:\\.tex)?)[}]$",
                   "\\1",
                   lines_after_begin_document[grepl("^\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{](.*(\\.tex)?)[}]$",
                                                    lines_after_begin_document,
                                                    perl = TRUE)],
                   perl = TRUE)
    
    if (length(inputs) != inputs_in_doc){
      stop("Unable to parse inputs. Check they are all of the form \\input{filename}.", "\n", 
           "\\input inside a comment will NOT be ignored.")
    }
    
    if (append.tex){
      inputs <- paste0(inputs, ".tex")
    }
    
    return(inputs)
  } else {
    return(NULL)
  }
}
