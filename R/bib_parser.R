#' Functions for parsing .bib files
#' @name bib_parser
#' @import data.table
#' @importFrom dplyr if_else
#' @importFrom dplyr coalesce
#' @param file.bib \code{.bib} file.
#' @param to_sort Include only author, title, year, and date.
#' @details \code{bib2DT} returns a \code{data.table} of the entries in \code{file.bib}. The function
#' \code{reorder_bib} rewrites \code{file.bib}, to put it in surname, year, title, line number order.
#' @export bib2DT

bib2DT <- function(file.bib, to_sort = FALSE){
  stopifnot(length(file.bib) == 1L)
  if (!grepl("\\.bib$", file.bib)){
    warning("File extension is not '.bib'.")
  }

  bib <-
    readLines(file.bib, encoding = "UTF-8") %>%
    # Avoid testing }\\s+$ rather than just == }
    trimws %>%
    .[!grepl("@Comment", ., fixed = TRUE)]
  is_at <- grepl("^@", bib, perl = TRUE)
  is_closing <- bib == "}"

  # We make assumptions about the structure
  # Namely that the closing brace for each entry
  # is the only character on its own line.
  n_ats <- sum(is_at)
  n_closing_braces <- sum(is_closing)

  if (n_ats != n_closing_braces){
    cat("@'s: ", n_ats, "\n",
        "}'s: ", n_closing_braces, "\n")
    stop("Unable to parse: Braces to close a bib entry should be on their own line.")
  }

  # All entries must have a nonempty key to be valid
  if (any(grepl("{,", bib[is_at], fixed = TRUE))){
    cat("At line: ", grep("{,", bib[is_at], fixed = TRUE)[[1]])
    stop("All bib entries must have a non-empty key.")
  }

  line_by_entry_no <- cumsum(is_at)
  keys <- gsub("^.*\\{(.*),$", "\\1", bib[is_at], perl = TRUE)
  
  if (uniqueN(keys) != n_ats){
    stop("Number of unique keys not equal to number of key entries.")
  }

  is_field <-
    !(is_at | is_closing | bib == "")

  extract_field_from <- function(field, from){
    pattern <- sprintf("%s%s%s", "^", field, "\\s+[=]\\s+[{](.*)[}],?$")
    if_else(grepl(pattern, from, perl = TRUE),
            gsub(pattern = pattern,
                 "\\1",
                 from), # perl = TRUE slower
            NA_character_)
  }

  # CRAN NOTE avoidance
  Line_no <- Surname <- Year <- annote <- author <- booktitle <- chapter <- crossref <- edition <-
    editor <- endyear <- howpublished <- institution <- intra_key_line_no <- line_no <-
    note <- number <- organization <- pages <- publisher <- related <- school <- series <- title <-
    type <- volume <- Year_as_date <- Date <- NULL
  
  bib_orig <- field_name <- NULL

  data.table(line_no = seq_along(bib),
             bib = bib,
             line_by_entry_no = line_by_entry_no,
             is_field = is_field,
             is_closing = is_closing) %>%
    .[, key := if_else(is_at,
                       gsub("^@[A-Za-z]+\\{(.*),$", "\\1", bib, perl = TRUE),
                       NA_character_)] %>%
    .[, key := zoo::na.locf(key, na.rm = FALSE)] %>%
    .[, author := extract_field_from("author", bib)] %>%
    .[, title  := extract_field_from("title", bib)] %>%
    .[, year   := extract_field_from("year", bib)] %>%
    .[, date   := extract_field_from("date", bib)] %>%
    {
      dot <- .
      if (!to_sort){
        out <-
          dot %>%
          .[, address := extract_field_from("address", bib)] %>%
          .[, annote   := extract_field_from("annote", bib)] %>%
          .[, booktitle   := extract_field_from("booktitle", bib)] %>%
          .[, booktitle   := extract_field_from("booktitle", bib)] %>%
          .[, chapter   := extract_field_from("chapter", bib)] %>%
          .[, crossref   := extract_field_from("crossref", bib)] %>%
          .[, options   := extract_field_from("options", bib)] %>%
          .[, related   := extract_field_from("related", bib)] %>%
          .[, edition   := extract_field_from("edition", bib)] %>%
          .[, editor   := extract_field_from("editor", bib)] %>%
          .[, howpublished   := extract_field_from("howpublished", bib)] %>%
          .[, institution   := extract_field_from("institution", bib)] %>%
          .[, month   := extract_field_from("month", bib)] %>%
          .[, note   := extract_field_from("note", bib)] %>%
          .[, number   := extract_field_from("number", bib)] %>%
          .[, organization   := extract_field_from("organization", bib)] %>%
          .[, pages   := extract_field_from("pages", bib)] %>%
          .[, publisher   := extract_field_from("publisher", bib)] %>%
          .[, school   := extract_field_from("school", bib)] %>%
          .[, series   := extract_field_from("series", bib)] %>%
          .[, type   := extract_field_from("type", bib)] %>%
          .[, volume   := extract_field_from("volume", bib)] %>%
          .[, url   := extract_field_from("url", bib)] %>%
          .[, edition   := extract_field_from("edition", bib)]
      } else {
        out <- dot
      }
      out
    } %>%
    # 1999/2015 --> 1999
    .[, date := if_else(grepl("/", date, fixed = TRUE),
                        substr(date, 0, 4),
                        date)] %>%
    .[, Year_as_date := if_else(grepl("[0-9]{4}", year),
                                paste0(year, "-01-01"),
                                year)] %>%
    .[, Date := coalesce(date, Year_as_date)] %>%
    .[, Surname := if_else(!is.na(author),
                           # If protected, just use as-is
                           # recalling that the outer braces have been removed already
                           if_else(grepl("^[{]", author, perl = TRUE),
                                   gsub("[{}]", "", author, perl = TRUE),
                                   # Daley, John
                                   if_else(grepl("^[A-Z][a-z]+,", author, perl = TRUE),
                                           author,
                                           rev_forename_surname_bibtex(author))
                           ),
                           # Sort first
                           NA_character_)] %>%
    .[, intra_key_line_no := seq_len(.N), by = "key"] %>%
    .[, Line_no := line_no] %>%
    .[, field_name := gsub("[^a-z]", "", gsub("[=].*$", "", bib, perl = TRUE), perl = TRUE)] %>%
    .[, field_name := if_else(is_at, "AT", field_name)] %>%
    .[, field_name := if_else(is_closing, "}", field_name)] %>%
    .[, field_name := factor(field_name,
                             levels = c("AT", "author", "title", "year", "date", "address", "annote", 
                                        "booktitle", "chapter", "crossref", "options", "related", "edition", 
                                        "editor", "howpublished", "institution", "month", "note", "number", 
                                        "organization", "pages", "publisher", "school", "series", "type", 
                                        "volume", "url", "Year_as_date", "Date", "Surname", "intra_key_line_no", 
                                        "Line_no", "}"), 
                             ordered = TRUE)] %>%
    .[, bib_orig := bib] %>%
    .[, lapply(.SD, zoo::na.locf, na.rm = FALSE, fromLast = FALSE), by = "key", .SDcols = author:bib_orig] %>%
    .[, lapply(.SD, zoo::na.locf, na.rm = FALSE, fromLast = TRUE) , by = "key", .SDcols = author:bib_orig] %>%
    setorder(Surname, Date, title, field_name, Line_no) %>%
    .[]
}

#' @rdname bib_parser
#' @param outfile.bib File to write the reordered bib to. Defaults to \code{file.bib}.
#' @export reorder_bib
reorder_bib <- function(file.bib, outfile.bib = file.bib){
  out_no <- bib2DT(file.bib, to_sort = TRUE)[["Line_no"]]
  y <- readLines(file.bib, encoding = "UTF-8")
  writeLines(y[out_no], outfile.bib, useBytes = TRUE)
}

