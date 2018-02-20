#' Check mismatched parentheses
#' @noRd

check_unclosed_parentheses <- function(filename,
                                       rstudio = FALSE,
                                       tex_lines = NULL,
                                       verbose = FALSE) {
  if (is.null(tex_lines)) {
    tex_lines <- read_lines(filename)
  }
  tex_lines_nchar <- nchar(tex_lines)
  
  # equations like \(x^2 + y^2 = h^2\)
  tex_lines_nomath <- gsub("\\\\\\(|\\\\\\)", "\\$", tex_lines, perl = TRUE)
  
  if (sum(stri_count_fixed(tex_lines, pattern = "(")) != 
      sum(stri_count_fixed(tex_lines, pattern = ")"))) {
    tex_lines_split <- strsplit(tex_lines_nomath, "", fixed = TRUE)
    
    tex_line_chars <- unlist(tex_lines_split)
    
    final_parentheses <- cumsum(tex_line_chars == "(") - cumsum(tex_line_chars == ")")
    
    line_no <- NULL
    parsed <- 
      data.table(char = tex_line_chars,
                 char_no = seq_along(tex_line_chars),
                 line_no = rep(seq_along(tex_lines), times = tex_lines_nchar),
                 parenthesis_group = final_parentheses)
    
    if (last(final_parentheses) > 0L) {
      # Opens
      char_no <- max(which(and(final_parentheses == last(final_parentheses),
                               shift(final_parentheses, fill = 0L) == 0L)))
      stop(parsed[char_no][["line_no"]], " contains parenthesis that does not close.")
    } else {
      # Closes
      wrong_i <- which.min(final_parentheses)
      wrong_line_no <- parsed[wrong_i][["line_no"]]
      wrong_char_no <- parsed[wrong_i][["char_no"]]
      if (verbose) {
        print(parsed[line_no == wrong_line_no])
      }
      column <- nrow(parsed[line_no == wrong_line_no][char_no <= wrong_char_no])
      context <- paste0(utils::head(parsed[line_no == wrong_line_no][["char"]], 
                                    column + 80L), 
                        collapse = "")
      
      report2console(file = filename,
                     line_no = wrong_line_no,
                     column = column,
                     context = context, 
                     rstudio = rstudio && interactive(),
                     error_message = "Unexpected closing parenthesis.",
                     advice = paste("Did you forget to open a parenthesis matching this one?",
                                    "or did you insert this parenthesis accidentally?"))
    }
    
    
    
    
    
  }
}
