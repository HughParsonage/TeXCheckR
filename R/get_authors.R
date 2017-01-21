#' Extract authors from report
#' @param filename The filename whose preamble contains the names of the authors.
#' @return The names of Grattan staff who were authors in \code{filename}.
#' @export

get_authors <- function(filename){
  lines <- readLines(filename, encoding = "UTF-8")
  file_path <- dirname(filename)

  lines_before_begin_document <-
    lines[1:grep("\\begin{document}", lines, fixed = TRUE)]
  
  if (any(grepl("\\input", lines_before_begin_document, fixed = TRUE))){
    # Ensure the only input in acknowledgements is tex/acknowledgements
    acknowledgements <- 
      paste0(lines_before_begin_document, collapse = " ") %>%
      gsub("^.*\\\\(acknowledgements)", "", ., perl = TRUE)
    
    if (any(grepl("\\input", acknowledgements, fixed = TRUE))){
      inputs <-
        gsub("^.*\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{]([^\\}]+(?:\\.tex)?)[}].*$",
             "\\1",
             acknowledgements,
             perl = TRUE)
      
      if (inputs[[1]] != "tex/acknowledgements"){
        stop("The only permitted \\input in \\acknowledgements is \\input{tex/acknowledgements}")
      }
      
      lines_before_begin_document <- 
        c(lines_before_begin_document,
          readLines(file.path(file_path, "./tex/acknowledgements.tex"),
                    encoding = "UTF-8",
                    warn = FALSE))
    }
  }

  regex_possible_names <-
    sprintf("(%s)",
            paste0("(?:",
                   Grattan_staff[["name"]],
                   ")",
                   collapse = "|"))

  possible_lines_split <-
    grep(regex_possible_names, lines_before_begin_document, value = TRUE) %>%
    # Need to split names.
    strsplit(split = paste0("\\W(?=", regex_possible_names, ")"),
             perl = TRUE) %>%
    unlist %>%
    grep(regex_possible_names,
         .,
         value = TRUE)

  possible_names <-
    possible_lines_split %>%
    gsub(sprintf("^.*(%s).*$",
                 regex_possible_names),
         "\\1",
         .) %>%
    unique

  possible_names
}
