

check_sentence_ending_periods <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- 
    readLines(filename, encoding = "UTF-8", warn = FALSE) %>%
    .[!isR_line_in_knitr(.)] %>%
    gsub("((?<!(\\\\))%).*$", "", ., perl = TRUE) %>%
    trimws
  
  if (any(grepl("[A-Z]\\.\\s+[A-Z]", lines, perl = TRUE)) || 
      any(and(lag(grepl("[A-Z]\\.$", lines, perl = TRUE)),
              grepl("^[A-Z]", lines, perl = TRUE)))){
    
    line_nos <- 
      which(or(grepl("[A-Z]\\.\\s+[A-Z]", lines, perl = TRUE),
               and(lag(grepl("[A-Z]\\.$", lines, perl = TRUE)),
                   grepl("^[A-Z]", lines, perl = TRUE))))
    
    .report_error(line_no = line_nos[[1]], 
                  context = lines[line_nos[[1]]], 
                  error_message = paste0("Sentences which end with a capital letter ",
                                         "need to be signalled with a sentence-ending period. (\\@.)"))
    stop(paste0("Sentences which end with a capital letter ",
                "need to be signalled with a sentence-ending period. (\\@.)"))
  }
}

