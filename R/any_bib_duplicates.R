#' Are any bib entries duplicated?
#' @param bib.file File to check for duplicates
#' @export 


any_bib_duplicates <- function(bib.file){
  key <- field <- NULL
  bibDT <- 
    fread_bib(bib.file) %>% 
    .[field != "absract"] %>%
    dcast.data.table(formula = key ~ field, value.var = "value")
  
  if ("origyear" %in% names(bibDT)){  
    bibDT <- bibDT[!is.na(origyear)]
  }
  
  if ("date" %notin% names(bibDT)){
    bibDT[, date := NA_character_]
  }
  
  bibDT %>%
    .[, Year := if_else(is.na(year),
                        if_else(is.na(date),
                                NA_integer_,
                                as.integer(substr(date, 0, 4))), 
                        as.integer(year))] %>%
    .[, Author := rev_forename_surname_bibtex(author)] %>%
    .[, Title := tolower(title)]
  
  if (anyDuplicated(bibDT, by = c("Author", "Year", "Title"))){
    dups_head <- duplicated(bibDT, by = c("Author", "Year", "Title"))
    dups_tail <- duplicated(bibDT, by = c("Author", "Year", "Title"), fromLast = TRUE)
    print(bibDT[dups_tail | dups_head])
    stop("Possible duplicate entries in bibliography.")
  }
  invisible(NULL)
}
