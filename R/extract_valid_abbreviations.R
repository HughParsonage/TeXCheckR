#' Extract valid abbreviations and initialisms
#' @param lines Lines to extract
#' @return Character vector of abbreviations of the form (ABC)
#' @details Only 'valid' abbreviations are extracted, viz. those abbreviations of the form \code{(ABC)} where the first letters
#' of the three words before
#' (excluding some common words like \code{of}, \code{and}, etc.) are 'a', 'b', 'c'.


extract_validate_abbreviations <- function(lines){
  # Note the inner (capturing) parentheses
  abbrev_pattern <- "\\(([A-Z][A-Za-z]*[A-Z])\\)"

  lines_w_abbrev <- grep(abbrev_pattern, lines, perl = TRUE, value = TRUE)
  if (not_length0(lines_w_abbrev)){
    lines_w_abbrev_last <-
      lines_w_abbrev %>%
      gsub("[{,.]", " ", x = ., perl = TRUE) %>%
      gsub("\\s+(?:(?:of)|(?:and)|(?:the))\\s+", " ", x = ., perl = TRUE) %>%
      gsub("\\s+", " ", x = ., perl = TRUE) %>%
      # Split if ends with A-Z)
      strsplit(split = "(?<=([A-Z]\\)))", perl = TRUE) %>%
      unlist %>%
      .[grepl(paste0(abbrev_pattern, "$"), ., perl = TRUE)]

    lines_w_abbrev_last_incl_stops <-
      lines_w_abbrev %>%
      gsub("[{,.]", " ", x = ., perl = TRUE) %>%
      # Exclude this line:
      # gsub("\\s+(?:(?:of)|(?:and)|(?:the))\\s+", " ", x = ., perl = TRUE) %>%
      gsub("\\s+", " ", x = ., perl = TRUE) %>%
      # Split if ends with A-Z)
      strsplit(split = "(?<=([A-Z]\\)))", perl = TRUE) %>%
      unlist %>%
      .[grepl(paste0(abbrev_pattern, "$"), ., perl = TRUE)]

    NN <- abbrev <- expected_abbrev <-
      expected_abbrev_with_stops <- figs_tbls_not_refd <- nchars_abbrev <- prefix <- prefix_incl_stops <- NULL
    data.table(
      line = lines_w_abbrev_last,
      abbrev = gsub(paste0("^(.*)", abbrev_pattern, "$"), "\\2", lines_w_abbrev_last, perl = TRUE),
      prefix = trimws(gsub(paste0("^(.*)", abbrev_pattern, "$"), "\\1", lines_w_abbrev_last, perl = TRUE)),
      prefix_incl_stops = trimws(gsub(paste0("^(.*)", abbrev_pattern, "$"), "\\1", lines_w_abbrev_last_incl_stops, perl = TRUE))
      ) %>%
      # Look at the n words previous where n is the nchar
      .[, nchars_abbrev := nchar(abbrev)] %>%
      .[, NN := seq.int(1, .N)] %>%
      .[, expected_abbrev := lapply(strsplit(prefix, split = " "),
                                    function(words){
                                      get_initials_from_words <- 
                                        seq.int(to = length(words), length.out = nchars_abbrev)
                                      # Don't look back further than the number of words in line
                                      # (else you'll get negative subscript errors)
                                      if (all(get_initials_from_words > 0)){
                                        out <- 
                                          words[get_initials_from_words] %>%
                                          substr(0, 1) %>%
                                          paste0(collapse = "") %>%
                                          toupper
                                      } else {
                                        out <- ""
                                      }
                                      out
                                    }) %>%
          unlist#
        , by = NN] %>%
      .[, expected_abbrev_with_stops := lapply(strsplit(prefix_incl_stops, split = " "),
                                               function(words){
                                                 get_initials_from_words <- 
                                                   seq.int(to = length(words), length.out = nchars_abbrev)
                                                 # Don't look back further than the number of words in line
                                                 # (else you'll get negative subscript errors)
                                                 if (all(get_initials_from_words > 0)){
                                                   out <- 
                                                     words[get_initials_from_words] %>%
                                                     substr(0, 1) %>%
                                                     paste0(collapse = "") %>%
                                                     toupper
                                                 } else {
                                                   out <- ""
                                                 }
                                                 out
                                               }) %>%
          unlist#
        , by = NN] %>%
      .[toupper(abbrev) == expected_abbrev | toupper(abbrev) == expected_abbrev_with_stops] %>%
      .[["abbrev"]]
  } else {
    invisible(NULL)
  }
}
