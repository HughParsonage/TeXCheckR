#' Are any bib entries duplicated?
#' @param bib.file File to check for duplicates
#' @export 


any_bib_duplicates <- function(bib.file){
  key <- field <- NULL
  bibDT <- 
    fread_bib(bib.file) %>% 
    .[field != "absract"] %>%
    dcast.data.table(formula = key ~ field, value.var = "value") %>%
    .[!is.na(origyear)] %>%
    .[, Year := if_else(is.na(year),
                        if_else(is.na(date),
                                NA_integer_,
                                as.integer(substr(date, 0, 4))), 
                        as.integer(year))] %>%
    .[, Author := rev_forename_surname_bibtex(author)] %>%
    .[, Title := tolower(title)]
  
  if (anyDuplicated(bibDT, by = c("Author", "Year", "Title"))){
    print(bibDT[anyDuplicated(bibDT, by = c("Author", "Year", "Title"))])
    stop("Possible duplicate entries in bibliography.")
  }
  invisible(NULL)
}
