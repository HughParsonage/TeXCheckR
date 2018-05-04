#' Read a LaTeX document
#' @param file_root The root of the TeX file.
#' @export

read_tex_document <- function(file_root) {
  filename <- file_root
  file_path <- function(x) file.path(dirname(filename), x)
  project_lines_0 <- strip_comments(read_lines(filename))
  project_lines_list <- as.list(project_lines_0)
  
  inputs_in_doc <-
    grep("\\\\(?:(?:input)|(?:include(?!(graphics))))\\{",
         project_lines_0,
         perl = TRUE)
  
  
  if (length(inputs_in_doc)) {
    include_lines <- grep("\\include{", project_lines_0, fixed = TRUE)
    
    # include invokes a page break anyway
    project_lines <- 
      gsub("\\include{", 
           "\\input{", 
           project_lines_0, 
           fixed = TRUE)
    
    for (L in grep("\\input{", project_lines, fixed = TRUE)) {
      #
      # \documentclass{article}
      # \input{preamble}              ==> c("\\relax", "\\relax")
      # \begin{document}
      # \input{A}\input{B}\input{A}   ==> c("A", "B", "A") ==> "ABA"
      # \input{C}\input{D}\input{C}   ==> c(c("c", "c"), "d", c("c", "c"))  ['as-is']
      # \input{E}                     ==> c("e", "e", "e")
      # \end{document}
      
      
      inputs_in_L <- 
        project_lines[L] %>%
        extract_mandatory_LaTeX_argument("input") %>%
        .subset2("extract")
      
      for (input_x in inputs_in_L) {
        if (endsWith(project_lines[L], paste0("\\input{", input_x, "}"))) {
          project_line_L <-
            strsplit(paste0(project_lines[L], " "), # for strsplit last char
                     split = paste0("\\input{", input_x, "}"),
                     fixed = TRUE)[[1L]]
        } else {
          project_line_L <-
            strsplit(project_lines[L],
                     split = paste0("\\input{", input_x, "}"),
                     fixed = TRUE)[[1L]]
        }
        input.tex <-
          if_else(endsWith(input_x, ".tex"), 
                  input_x,
                  paste0(input_x, ".tex"))
        
        
        read_tex_i <- read_tex_document(file_path(input.tex))
        
        # Need to distinguish between \input{A}\input{A} when A is a tex file with a single line
        if (length(read_tex_i) > 1L) {
          for (i in seq_len(length(project_line_L) - 1L)) {
            project_line_L <- 
              c(project_line_L[seq_len(i)], 
                read_tex_i,
                project_line_L[i + 1L])
          }
          # These are artefacts from strsplit
          if (first(project_line_L) == "" && 
              last(project_line_L) == " ") {
            project_line_L <- project_line_L[2:(length(project_line_L) - 1)]
          }
        } else {
          for (i in seq_len(length(project_line_L) - 1L)) {
            project_line_L <-
              paste0(c(project_line_L[seq_len(i)], 
                       read_tex_i,
                       if (!identical(project_line_L[i + 1L], " ")) {
                         project_line_L[i + 1L]
                       }),
                     collapse = "")
          }
        }
      }
      project_lines_list[[L]] <- as.list(project_line_L)
    }
  }
  
  unlist(project_lines_list, use.names = FALSE)
}




