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
    inputs <- 
      lapply(inputs_in_doc, 
             function(x) {
               line_subfile <-
                 sub("\\include{",
                     "\\input{",
                     project_lines_0[x],
                     fixed = TRUE)
               
               subfile <- 
                 # Need to deal with \input s on the same line
                 # Problem if \b occurs natively but very unlikely.
                 strsplit(gsub("\\\\input\\{([^\\}]++)\\}",
                               "\\1\b",
                               line_subfile,
                               perl = TRUE),
                          "\b",
                          fixed = TRUE)[[1L]]
               subfile <- 
                 file.path(dirname(filename), 
                           sprintf("%s.tex", tools::file_path_sans_ext(subfile)))
               read_tex_root(subfile)
             })
    project_lines_0 <- as.list(project_lines_0)
    for (i in seq_along(inputs_in_doc)) {
      project_lines_0[[inputs_in_doc[i]]] <- inputs[[i]]
    }
  }
  
  unlist(project_lines_0, use.names = FALSE)
}

