

extract_authors_from_bib <- function(bib.file){
  readLines(bib.file, encoding = "UTF-8", warn = FALSE) %>%
    grep("^\\s+author", ., value = TRUE, perl = TRUE) %>%
    gsub("^.*[=]\\s+", "", ., perl = TRUE) %>%
    gsub("[\\{\\},]", "", ., perl = TRUE) %>%
    strsplit(split = " ", fixed = TRUE) %>%
    unlist %>%
    unique %>%
    .[nchar(.) > 1L] 
    
    
}
