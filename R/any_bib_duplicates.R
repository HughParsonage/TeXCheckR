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
    bibDT <- bibDT[is.na(origyear)]
  }
  
  date <- NULL
  if ("date" %notin% names(bibDT)){
    bibDT[, date := NA_character_]
  }
  
  author <- NULL
  if ("author" %notin% names(bibDT)){
    bibDT[, author := NA_character_]
  }
  
  title <- NULL
  if ("title" %notin% names(bibDT)){
    bibDT[, title := NA_character_]
  }
  
  bibDT %>%
    .[, Year := if_else(is.na(year),
                        if_else(is.na(date),
                                NA_character_,
                                substr(date, 0, 4)), 
                        as.character(year))] %>%
    .[, Author := rev_forename_surname_bibtex(author)] %>%
    .[, Title := tolower(title)] %>%
    # ABS duplicate if identical without Australia
    .[, Title := if_else(Author == "ABS", 
                         
                         gsub(", australia,", ",", Title, fixed = TRUE), 
                         Title)]
  
  
  
  if (F && anyDuplicated(bibDT, by = c("Author", "Year", "Title"))){
    dups_head <- duplicated(bibDT, by = c("Author", "Year", "Title"))
    dups_tail <- duplicated(bibDT, by = c("Author", "Year", "Title"), fromLast = TRUE)
    DT_with_all_duplicates <- 
      bibDT %>%
      .[dups_tail | dups_head, .(key, Author, Title, date, year)] %>%
      .[order(Author, Title)]
    
    stopifnot(nrow(DT_with_all_duplicates) %% 2 == 0, nrow(DT_with_all_duplicates) > 1)
    for (dup in 1:(nrow(DT_with_all_duplicates) / 2)){
      if (dup == 6){
        break
      }
      cat("\n")
      print(DT_with_all_duplicates[c(2 * dup - 1, 2 * dup)], row.names = FALSE)
    }
    stop("Possible duplicate entries in bibliography. First 5 shown above.")
  }
  invisible(NULL)
}
