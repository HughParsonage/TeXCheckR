#' Bib file as data.table
#'
#' @import data.table
#' @importFrom dplyr if_else
#' @importFrom dplyr coalesce
#' @param file \code{.bib} file.
#' @export

bib2DT <- function(file){
  stopifnot(length(file) == 1L)
  if (!grepl("\\.bib$", file)){
    warning("File extension is not '.bib'.")
  }

  bib <-
    readLines(file, encoding = "UTF-8") %>%
    # Avoid testing }\\s+$ rather than just == }
    trimws
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

  is_field <-
    !(is_at | is_closing | bib == "")

  extract_field_from <- function(field, from){
    pattern <- sprintf("%s%s%s", "^", field, "\\s+[=]\\s+[{](.*)[}],?$")
    if_else(grepl(pattern, from, perl = TRUE),
            gsub(pattern = pattern,
                 "\\1",
                 from,
                 perl = TRUE),
            NA_character_)
  }

  # CRAN NOTE avoidance
  Line_no <- Surname <- Year <- annote <- author <- booktitle <- chapter <- crossref <- edition <-
    editor <- endyear <- howpublished <- institution <- intra_key_line_no <- line_no <-
    note <- number <- organization <- pages <- publisher <- related <- school <- series <- title <-
    type <- volume <- NULL

  data.table(line_no = seq_along(bib),
             bib = bib,
             line_by_entry_no = line_by_entry_no,
             is_field = is_field) %>%
    .[, key := if_else(is_at,
                       gsub("^@[A-Za-z]+\\{(.*),$", "\\1", bib, perl = TRUE),
                       NA_character_)] %>%
    .[, key := zoo::na.locf(key, na.rm = FALSE)] %>%
    # .[is_field] %>%
    .[, author := extract_field_from("author", bib)] %>%
    .[, title  := extract_field_from("title", bib)] %>%
    .[, year   := extract_field_from("year", bib)] %>%
    .[, date   := extract_field_from("date", bib)] %>%
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
    .[, edition   := extract_field_from("edition", bib)] %>%
    # Inference
    .[, endyear := gsub("^[0-9]{4}/([0-9]{4})$", "\\1", date, perl = TRUE)] %>%
    .[, Year := coalesce(year, endyear, substr(date, 0, 4))] %>%
    .[, Surname := if_else(!is.na(author),
                           # If protected, just use as-is
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
    .[, lapply(.SD, zoo::na.locf, na.rm = FALSE, fromLast = FALSE), by = "key", .SDcols = author:Line_no] %>%
    .[, lapply(.SD, zoo::na.locf, na.rm = FALSE, fromLast = TRUE) , by = "key", .SDcols = author:Line_no] %>%
    setorder(Surname, Year, title, intra_key_line_no) %>%
    .[]
}

reorder_bib <- function(bib, outbib){
  out_no <- bib2DT(bib)[["Line_no"]]
  readLines(bib, encoding = "UTF-8") %>%
    .[out_no] %>%
    writeLines(outbib, useBytes = TRUE)
}

