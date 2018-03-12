#' Are any bib entries duplicated?
#' @param bib.files Files to check for duplicates
#' @param .report_error How errors should be logged.
#' @param rstudio Use the RStudio API?
#' @details This function is very fastidious about the format of \code{bib.files}.
#' Run \code{\link{lint_bib}} (noting that this will overwrite your bibliography) if it complains.
#' 
#' This function finds exact duplicates in the author title date/year and volume fields.
#' Note that it is not possible in general to detect actual duplicates; you will still
#' need to inspect the printed bibliography. 
#' @return Called for its side-effect. If duplicates are detected, the first six are printed as a \code{data.table};
#' otherwise, \code{NULL}, invisibly.
#' @export 


any_bib_duplicates <- function(bib.files, .report_error, rstudio = FALSE) {
  if (missing(.report_error)){
    .report_error <- function(...) report2console(..., rstudio = rstudio)
  }
  
  .fread_bib <- function(file.bib) {
    fread_bib(file.bib) %>% 
      .[, bib_file := file.bib]
  }
  
  KEY <- key <- field <- bib_file <- NULL
  bibDT <- 
    lapply(bib.files, .fread_bib) %>% 
    rbindlist(use.names = TRUE, fill = TRUE) %>% 
    .[, KEY := toupper(key)]
  
  dups <- NULL
  duplicate_fields <-
    bibDT[, .(dups = anyDuplicated(field)), by = key]
  
  if (any(duplicate_fields[["dups"]])){
    keys <-
      duplicate_fields %>%
      .[as.logical(dups)] %>%
      .[["key"]]
    
    n_keys <- length(keys)
    
    if (n_keys <= 5L){
      top_keys <- keys
      stop("Duplicate fields found in ", paste0(keys, collapse = " "), ".")
    } else {
      top_keys <- keys[1:5]
      if (n_keys == 6L){
        stop("Duplicate fields found in ", paste0(keys, collapse = " "), ".")
      } else {
        stop("Duplicate fields found in ", paste0(keys, collapse = " "), " and ", n_keys - 5L, " others.")
      }
    }
  }
  
  orig_bibDT <- bibDT
  
  bibDT <-
    bibDT %>%
    .[field != "abstract"] %>%
    dcast.data.table(formula = key ~ field, value.var = "value")
  
  if ("origyear" %in% names(bibDT)) {
    origyear <- NULL
    bibDT <- bibDT[is.na(origyear)]
  }
  
  date <- NULL
  if ("date" %notchin% names(bibDT)) {
    bibDT[, date := NA_character_]
  }
  
  author <- NULL
  if ("author" %notchin% names(bibDT)) {
    bibDT[, author := NA_character_]
  }
  
  title <- NULL
  if ("title" %notchin% names(bibDT)) {
    bibDT[, title := NA_character_]
  }
  
  volume <- NULL
  if ("volume" %notchin% names(bibDT)) {
    bibDT[, volume := NA_character_]
  }
  
  Author <- Title <- Year <- NULL
  bibDT %>%
    .[, Year := if_else(is.na(year),
                        if_else(is.na(date),
                                NA_character_,
                                substr(date, 0L, 4L)), 
                        as.character(year))] %>%
    .[, Author := rev_forename_surname_bibtex(author)] %>%
    .[, Title := tolower(title)] %>%
    # ABS duplicate if identical without Australia
    .[Author == "ABS", Title := gsub(", australia,", ",", Title, fixed = TRUE)]
  
  
  
  if (anyDuplicated(bibDT, by = c("Author", "Year", "Title", "volume"))){
    dups_head <- duplicated(bibDT, by = c("Author", "Year", "Title", "volume"))
    dups_tail <- duplicated(bibDT, by = c("Author", "Year", "Title", "volume"), fromLast = TRUE)
    DT_with_all_duplicates <- 
      bibDT %>%
      .[dups_tail | dups_head, .(key, Author, Title, date, year)] %>%
      .[order(Author, Title)]
    
    stopifnot(nrow(DT_with_all_duplicates) %% 2L == 0L,
              nrow(DT_with_all_duplicates) > 1L)
    
    .report_error(line_no = NULL,
                  context = "Possible duplicates in bibliographies.",
                  error_message = "Possible duplicates in bibliography.")
    
    for (dup in 1:(nrow(DT_with_all_duplicates) / 2)){
      if (dup == 6){
        break
      }
      cat("\n")
      print(DT_with_all_duplicates[c(2 * dup - 1, 2 * dup)], row.names = FALSE)
    }
    stop("Possible duplicate entries in bibliography. First 5 shown above.")
  }
  
  # CRAN NOTE avoidance
  AUTHORS <- authors <- bad <- bib_file <- line_no <- key <- value <- NULL
  
  # Check for authors differing only by upper/lowercase
  
  authors <- 
    orig_bibDT %>%
    .[field == "author", .(bib_file, line_no, key, value)] %>%
    .[, authors := rev_forename_surname_bibtex(value)] %>%
    .[, AUTHORS := toupper(authors)] %>%
    # Obviously duplicate authors are fine, but not if
    # they aren't apparent duplicates
    .[, bad := duplicated(AUTHORS) & !duplicated(authors)]
  
  if (any(authors[["bad"]])) {
    
    authors_with_dups <- 
      authors[or(duplicated(AUTHORS) & !duplicated(authors),
                 duplicated(AUTHORS, fromLast = TRUE) & !duplicated(authors, fromLast = TRUE))] %>%
      .[order(AUTHORS, bib_file, line_no)]
    
    first_author_with_dup <- authors_with_dups[1:2]
    n_first_author_files <- uniqueN(first_author_with_dup[["bib_file"]])
    
    # If first author is dup in the same file, use that file,
    # else use the second file (where the dup is more likely?)
    if (n_first_author_files == 1L) {
      the_file <- first_author_with_dup[["bib_file"]]
      line_no <- first_author_with_dup[["line_no"]]
    } else {
      the_file <- first_author_with_dup[["bib_file"]][2L]
      line_no <- first_author_with_dup[["line_no"]][2L]
    }
    
    print(authors_with_dups[(bad), .(bib_file, line_no, key, value, authors)])
    
    
    .report_error(file = the_file,
                  line_no = line_no,
                  error_message = "Same author used with inconsistent upper/lowercase.", 
                  advice = "Ensure the entries above are used with consistent case so ibid/idem's are respected.")
    stop("Same author used with inconsistent upper/lowercase.")
  }
  
  invisible(NULL)
}
