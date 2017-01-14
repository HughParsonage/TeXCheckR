

get_authors <- function(filename){
  lines <- readLines(filename, encoding = "UTF-8")
  
  lines_before_begin_document <- 
    lines[1:grep("\\begin{document}", lines, fixed = TRUE)]
  
  regex_possible_names <-
    sprintf("(%s)", 
            paste0("(?:", 
                   Grattan_staff, 
                   ")",
                   collapse = "|"))
  
  possible_lines_split <- 
    grep(regex_possible_names, lines_before_begin_document, value = TRUE) %>%
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
