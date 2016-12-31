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

  bib <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
  bib <- bib[!grepl("% Valid", bib, fixed = TRUE)]

  # Abbreviated names
  if (any(grepl("^\\s+(author).*((Productivity Commission)|((Australian)? Bureau of Statistics))", bib, perl = TRUE))){
    first_bad <-
      grep(paste0("^\\s+(author).*",
                  "(",
                  "(Productivity Commission)",
                  "|",
                  "((Australian)? Bureau of Statistics)",
                  "|",
                  "(Australian Labor Party)",
                  "|",
                  "(Australian Institute of Health and Welfare)",
                  "|",
                  "(Word Health Organi[sz]ation)",
                  ")"),
           bib,
           perl = TRUE,
           value = TRUE) %>%
      .[1]
    cat(first_bad)
    stop("Institutional authors should be abbreviated.")
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

  should_be_Articles <-
    grep(paste0("^\\s+(url).*",
                "(",
                "((theguardian)|(afr)\\.com)",
                "|",
                "(((theaustralian)|(theage)|(smh)|(canberratimes))\\.com\\.au)",
                "|",
                "(theconversation\\.((edu\\.au)|(com)))",
                "|",
                "(insidestory\\.org\\.au)",
                ")"),
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
    stop("URL suggests the article type should be used.")
  }

  # dois should not include the top-level URL
  if (any(grepl("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE))){
    cat(grep("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE, value = TRUE)[[1]])
    stop("DOI entries must be in the form", "\n\t",
         "10.1787/9789264229945-en", "\n",
         "not", "\n\t",
         "http://dx.doi.org/10.1787/9789264229945-en")
  }

  invisible(NULL)
}
