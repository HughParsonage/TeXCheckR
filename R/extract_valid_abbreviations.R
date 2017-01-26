#' Extract valid abbreviations and initialisms
#' @param lines Lines to extract
#' @return Character vector of abbreviations of the form (ABC)
#' @details Only 'valid' abbreviations are extracted, viz. those abbreviations of the form \code{(ABC)} where the first letters
#' of the three words before
#' (excluding some common words like \code{of}, \code{and}, etc.) are 'a', 'b', 'c'.


extract_validate_abbreviations <- function(lines){
  lines_w_abbrev <- grep("\\([A-Z]+\\)", lines, perl = TRUE, value = TRUE)
  if (not_length0(lines_w_abbrev)){
    lines_w_abbrev_last <- 
      lines_w_abbrev %>%
      gsub("[{,.]", " ", x = ., perl = TRUE) %>%
      gsub("\\s+(?:(?:of)|(?:and)|(?:the))\\s+", " ", x = ., perl = TRUE) %>%
      gsub("\\s+", " ", x = ., perl = TRUE) %>%
      # Split if ends with A-Z)
      strsplit(split = "(?<=([A-Z]\\)))", perl = TRUE) %>%
      unlist %>%
      .[grepl("\\([A-Z]+\\)$", ., perl = TRUE)]
    
    NN <- abbrev <- expected_abbrev <- figs_tbls_not_refd <- nchars_abbrev <- prefix <- NULL
    data.table(
      line = lines_w_abbrev_last,
      abbrev = gsub("^(.*)\\(([A-Z]+)\\)$", "\\2", lines_w_abbrev_last, perl = TRUE),
      prefix = trimws(gsub("^(.*)\\(([A-Z]+)\\)$", "\\1", lines_w_abbrev_last, perl = TRUE))) %>%
      # Look at the n words previous where n is the nchar
      .[, nchars_abbrev := nchar(abbrev)] %>%
      .[, NN := seq.int(1, .N)] %>%
      .[, expected_abbrev := lapply(strsplit(prefix, split = " "), function(words){
        toupper(paste0(substr(words[seq.int(to = length(words), length.out = nchars_abbrev)], 0, 1), 
                       collapse = ""))
      }) %>% unlist, by = NN] %>% 
      .[abbrev == expected_abbrev] %>%
      .[["abbrev"]]
  } else {
    invisible(NULL)
  }
}
