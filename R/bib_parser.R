#' Functions for parsing .bib files
#' @name bib_parser
#' @param file.bib \code{.bib} file.
#' @param to_sort Include only author, title, year, and date.
#' @param check.dup.keys If \code{TRUE}, the default, return error if any bib keys are duplicates.
#' @param strip.braces If \code{TRUE}, the default, braces in fields are removed. 
#' @details \code{bib2DT} returns a \code{data.table} of the entries in \code{file.bib}. The function
#' \code{reorder_bib} rewrites \code{file.bib}, to put it in surname, year, title, line number order.
#' @export bib2DT fread_bib

fread_bib <- function(file.bib, check.dup.keys = TRUE, strip.braces = TRUE) {
  stopifnot(length(file.bib) == 1L)
  if (!endsWith(file.bib, ".bib")) {
    warning("File extension is not '.bib'.")
  }

  bib <-
    read_lines(file.bib) %>%  # Consider: fread("~/Road-congestion-2017/bib/Transport.bib", sep = "\n", fill = TRUE, encoding = "UTF-8", header = FALSE) 
    # Avoid testing }\\s+$ rather than just == }
    stri_trim_both %>%
    .[!startsWith(., "@Comment")]
  
  is_at <- startsWith(bib, "@")
  is_closing <- bib == "}"

  sep <- NULL
  # Can't use = as separator (almost certainly occurs in a URL)
  # Try these:
  for (sep_candidate in c("\t", "^", "|")){
    if (!any(grepl(sep_candidate, bib, fixed = TRUE))){
      sep <- sep_candidate
      break
    }
  }
  if (is.null(sep_candidate)){
    stop("No suitable separator found for bibliography file. That is, all candidates tried already appeared in the file")
  }

  bib_just_key_and_fields <- bib
  bib_just_key_and_fields[is_closing] <- ""
  bib_just_key_and_fields[is_at] <- sub("@", "key = ", bib_just_key_and_fields[is_at], fixed = TRUE)

  # Make sure the sep is detected (in case of >author   ={John Daley}<)
  bib_just_key_and_fields <- sub("={", "= {", bib_just_key_and_fields, fixed = TRUE)
  bib_just_key_and_fields <- sub(" = ", sep, bib_just_key_and_fields, fixed = TRUE)
  used_line_nos <- which(nzchar(bib_just_key_and_fields))
  bib_just_key_and_fields <- bib_just_key_and_fields[used_line_nos]

  x <- line_no <- NULL
  bibDT <- setDT(list(line_no = used_line_nos,
                      x = bib_just_key_and_fields))

  field <- value <- NULL
  bibDT[, c("field", "value") := tstrsplit(x, sep, fixed = TRUE)]

  is_key <- NULL
  bibDT[, is_key := field == "key"]
  
  key_value <- NULL
  bibDT[(is_key), key_value := tolower(sub("^.*\\{", "", value, perl = TRUE))]
  
  if (check.dup.keys && anyDuplicated(stats::na.omit(bibDT[["key_value"]]))) {
    duplicates <- duplicated_rows(bibDT, by = "key_value")
    if (getOption("TeXCheckR.messages", TRUE)) {
      print(duplicates[, "bib_file" := file.bib])
    }
    report2console(file = file.bib,
                   line_no = if (!is.null(duplicates[["line_no"]])) first(duplicates[["line_no"]]),
                   error_message = "Duplicate bib key used.",
                   advice = paste0("Compare the two entries above. If they are identical, delete one. ", 
                                   "If they are distinct, choose a different bib key. ", 
                                   "(Note: keys are case-insensitive.)"))
    stop("Duplicate bib key used.")
  }
  
  bibDT[, key_value := NULL]

  key_line <- entry_type <- NULL
  bibDT[(is_key), key_line := value]
  bibDT[, key_line := zoo::na.locf(key_line, na.rm = FALSE)]
  bibDT <- bibDT[(!is_key)]
  bibDT[, x := NULL]

  bibDT[, lapply(.SD, stri_trim_both)]
  bibDT[, key_line := sub(",$", "", key_line, perl = TRUE)]
  bibDT[, c("entry_type", "key") := tstrsplit(key_line, "{", fixed = TRUE)]
  bibDT[, field := tolower(stri_trim_both(field))]
  if (strip.braces) {
    bibDT[, value := gsub("[{}]", "", value, perl = TRUE)]
  }
  bibDT[, value := sub(",$", "", value, perl = TRUE)]

  
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
  bibDT[, .(line_no, entry_type, key, field, value)]

}

#' @rdname bib_parser

bib2DT <- function(file.bib, to_sort = FALSE){
  stopifnot(length(file.bib) == 1L)
    if (!grepl("\\.bib$", file.bib)){
      warning("File extension is not '.bib'.")
    }

    bib <-
      read_lines(file.bib) %>%
      # Avoid testing }\\s+$ rather than just == }
      stri_trim_both %>%
      .[!grepl("@Comment", ., fixed = TRUE)]
    is_at <- substr(bib, 0L, 1L) == "@" #grepl("^@", bib, perl = TRUE)
    is_closing <- bib == "}"
    entry_no <- cumsum(is_at)
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

