#' Read a LaTeX document
#' @param file_root The root of the TeX file.
#' @export

read_tex_document <- function(file_root) {
  filename <- file_root
  project_lines_0 <- strip_comments(read_lines(filename))
  inputs_in_doc <-
    grep("\\\\(?:(?:input)|(?:include(?!(graphics))))\\{",
         project_lines_0,
         perl = TRUE)
  
  if (length(inputs_in_doc)) {
    input_files <- 
      project_lines_0 %>%
      .[inputs_in_doc] %>%
      gsub("\\include{", "\\input{", x = ., fixed = TRUE) %>%
      extract_LaTeX_argument("input") %>%
      .subset2("extract")
    
    input_files <- file.path(dirname(filename), input_files)
    input_files[!endsWith(input_files, ".tex")] <-
      paste0(input_files[!endsWith(input_files, ".tex")], ".tex")
    
    inputs <- lapply(input_files, read_tex_document)
    
    project_lines_0 <- as.list(project_lines_0)
    for (i in seq_along(inputs_in_doc)) {
      project_lines_0[[inputs_in_doc[i]]] <- inputs[[i]]
    }
  }
  
  unlist(project_lines_0, use.names = FALSE)
}

