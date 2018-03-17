

check_sentence_ending_periods <- function(filename,
                                          .report_error,
                                          ignore_hl = TRUE) {
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- 
    read_lines(filename) %>%
    .[!isR_line_in_knitr(.)] %>%
    gsub("((?<!(\\\\))%).*$", "", ., perl = TRUE) %>%
    stri_trim_both
  
  if (ignore_hl) {
    hl_lines <- grep("\\hl{", lines, fixed = TRUE)
    if (length(hl_lines)) {
      lines[hl_lines] <- 
        gsub("\\\\hl\\{[^\\}]++\\}", "", lines[hl_lines],
             perl = TRUE)
    }
  }
  
  if (any(grepl("[A-Z]\\.\\s+[A-Z]", lines, perl = TRUE)) || 
      any(and(lag(grepl("[A-Z]\\.$", lines, perl = TRUE)),
              grepl("^[A-Z]", lines, perl = TRUE)))){
    
    line_nos <- 
      which(or(grepl("[A-Z]\\.\\s+[A-Z]", lines, perl = TRUE),
               and(lag(grepl("[A-Z]\\.$", lines, perl = TRUE)),
                   grepl("^[A-Z]", lines, perl = TRUE))))
    
    first_line_no <- line_nos[[1]]
    if (grepl("[A-Z]\\.\\s+[A-Z]", lines[first_line_no], perl = TRUE)){
      context <- lines[first_line_no]
    } else {
      context <- paste0(lines[c(first_line_no - 1, first_line_no)], collapse = " ")
      first_line_no <- paste0(c(first_line_no - 1, first_line_no), collapse = "-")
    }
    
    .report_error(line_no = first_line_no, 
                  context = context, 
                  error_message = "Capital letter ends sentence, but sentence-ending period mark absent.",
                  advice = paste0("Sentences which end with a capital letter ",
                                  "need to be signalled with a sentence-ending period. (\\@.)"))
    stop(paste0("Sentences which end with a capital letter ",
                "need to be signalled with a sentence-ending period. (\\@.)"))
  }
}

