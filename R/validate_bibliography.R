#' Validate bibliography according Grattan style
#' @param path Containing the bib file.
#' @param file The bib file if specified.
#' @return \code{NULL} if bibliography validated.
#' @export


validate_bibliography <- function(path = ".", file = NULL){
  if (is.null(file)){
    bib_files <- dir(path = path, pattern = "\\.bib$", full.names = TRUE)
    stopifnot(length(bib_files) == 1L)
    bib_file <- bib_files[[1]]
  } else {
    bib_file <- file
  }

  bib <-
    readLines(bib_file, warn = FALSE, encoding = "UTF-8") %>%
    trimws %>%
    .[!grepl("% Valid", ., fixed = TRUE)]

  # Abbreviated names
  inst_pattern <-
    paste0("^\\s+(author).*",
           "(",
           "(Productivity Commission)",
           "|",
           "((Australian )?Bureau of Statistics)",
           "|",
           "(Australian Labor Party)",
           "|",
           "(Australian Institute of Health and Welfare)",
           "|",
           "(Word Health Organi[sz]ation)",
           ")")

  if (any(grepl(inst_pattern, bib, perl = TRUE))){
    first_bad <-
      grep(inst_pattern,
           bib,
           perl = TRUE,
           value = TRUE) %>%
      .[1]
    cat(first_bad)
    stop(cat(crayon::bgRed(symbol$cross)), "Institutional authors should be abbreviated.")
  }


  # Ensure URLs suggesting a newspaper article only
  # appear in @Article types:
  bib_just_keys_and_urls <-
    grep("^(@|(\\s+(url)))",
         bib,
         perl = TRUE,
         value = TRUE)

  urls_not_articles <- c("http://images.theage.com.au/file/2014/07/31/5639573/Help_interest_rate_options_report.pdf")

  bib_just_keys_and_urls <-
    gsub("http://images.theage.com.au/file/2014/07/31/5639573/Help_interest_rate_options_report.pdf",
         "",
         bib_just_keys_and_urls,
         fixed = TRUE)

  newspapers_pattern <-
    paste0("^(url).*",
           "(",
           "(((theguardian)|(afr))\\.com)",
           "|",
           "(((theaustralian)|(theage)|(smh)|(canberratimes)|(greatlakesadvocate))\\.com\\.au)",
           "|",
           "(theconversation\\.((edu\\.au)|(com)))",
           "|",
           "(insidestory\\.org\\.au)",
           ")")

  should_be_Articles <-
    grep(newspapers_pattern,
         bib_just_keys_and_urls,
         perl = TRUE) - 1

  arent_Articles_but_should_be <-
    !grepl("@Article",
           bib_just_keys_and_urls[should_be_Articles],
           fixed = TRUE)

  if (any(arent_Articles_but_should_be)){
    bib_just_keys_and_urls %>%
      .[should_be_Articles] %>%
      .[arent_Articles_but_should_be] %>%
      .[1] %>%
      cat
    stop(cat(crayon::bgRed(symbol$cross)), "URL suggests the article type should be used.")
  }

  # Once we have verified all are articles, check the right journal has been included.
  just_key_journal_urls <-
    bib %>%
    .[grepl("^(@|(journal)|(url))", ., perl = TRUE) | bib == "}"]

  both_url_and_journal <- entry_no <- group_by <- is_article <- is_newspaper <-
    journal <- journal_actual <- journal_from_url <- text <- NULL

  journal_actual_vs_journal_expected <-
    data.table(text = just_key_journal_urls) %>%
    .[, entry_no := cumsum(grepl("^@", text))] %>%
    .[, is_article := any(grepl("^@Article", text)), by = entry_no] %>%
    # Journal + URL occur iff
    # @
    # TRUE
    # TRUE
    # }
    .[, both_url_and_journal := .N == 4L, by = entry_no] %>%
    .[, is_newspaper := any(grepl(newspapers_pattern, text, perl = TRUE)), by = entry_no] %>%
    .[is_article & both_url_and_journal &is_newspaper] %>%
    .[, .(key = grep("^@Article", text, perl = TRUE, value = TRUE),
          url = gsub("^url.*[{](.*)[}],?$",
                     "\\1",
                     text[grepl("^url", text, perl = TRUE)],
                     perl = TRUE),
          journal_actual = gsub("^journal.*[{](.*)[}],?$",
                                "\\1",
                                text[grepl("^journal", text, perl = TRUE)],
                                perl = TRUE),
          journal_from_url = gsub(paste0("^.*(", newspapers_pattern, ").*$"),
                                  "\\3",
                                  text[grepl("^url", text, perl = TRUE)],
                                  perl = TRUE)),
      by = entry_no] %>%
    setkey(journal_from_url) %>%
    .[newspaper_by_url]

  incorrect_journal_entries <-
    journal_actual_vs_journal_expected[journal_actual != journal]

  if (nrow(incorrect_journal_entries) > 0){
    cat(crayon::bgRed(symbol$cross, "Inconsistent treatment of article journal.\n"))
    print(incorrect_journal_entries)
    stop("In entry", "\n\t",
         incorrect_journal_entries[1][["key"]], "\n\n",
         "I see:", "\n\t",
         "url = {", incorrect_journal_entries[1][["url"]], "}", "\n\n",
         "which suggests", "\n\t",
         "journal = {", incorrect_journal_entries[1][["journal"]], "} ,", "\n\n",
         "but\n\t",
         "journal = {", incorrect_journal_entries[1][["journal_actual"]], "} .")
  }


  # dois should not include the top-level URL
  if (any(grepl("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE))){
    cat(grep("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE, value = TRUE)[[1]])
    stop("DOI entries must be in the form", "\n\t",
         "10.1787/9789264229945-en", "\n",
         "not", "\n\t",
         "http://dx.doi.org/10.1787/9789264229945-en")
  }

  asDT <- bib2DT(bib_file)

  # Check no dates and year
  nrows_years_and_date <-
    asDT %>%
    .[!is.na(year) & !is.na(date)] %>%
    unique(by = "key") %>%
    nrow

  if (nrows_years_and_date > 0){
    asDT %>%
      .[!is.na(year) & !is.na(date)] %>%
      unique(by = "key") %>%
      print

    stop(cat(crayon::bgRed(symbol$cross)), "Date and year should not both appear in bibliography.")
  }

  invisible(NULL)
}
