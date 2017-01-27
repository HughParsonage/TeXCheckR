

extract_authors_from_bib <- function(bib.file){
  read_lines(bib.file) %>%
    grep("^\\s+author", ., value = TRUE, perl = TRUE) %>%
    gsub("^.*[=]\\s+", "", ., perl = TRUE) %>%
    gsub("[\\{\\},]", "", ., perl = TRUE) %>%
    strsplit(split = " ", fixed = TRUE) %>%
    unlist %>%
    unique %>%
    .[nchar(.) > 1L] 
    
    
}
