

check_unclosed_parentheses <- function(tex_lines) {
  tex_lines_nchar <- nchar(tex_lines)
  
  # equations
  tex_lines_nomath <- gsub("\\\\(|\\\\)", "\\$", tex_lines, perl = TRUE)
  
  if (sum(stri_count_fixed(tex_lines, pattern = "(")) != 
      sum(stri_count_fixed(tex_lines, pattern = ")"))) {
    tex_lines_split <- strsplit(tex_lines_nomath, "", fixed = TRUE)
    
    tex_line_chars <- unlist(tex_lines_split)
    
    final_parentheses <- cumsum(tex_line_chars == "(") - cumsum(tex_line_chars == ")")
    
    parsed <- 
      data.table(char = tex_line_chars,
                 char_no = seq_along(tex_line_chars),
                 line_no = rep(seq_along(tex_lines), times = tex_lines_nchar))
    
    char_no <- max(which(and(final_parentheses == last(final_parentheses),
                             shift(final_parentheses, fill = 0L) == 0L)))
    
    
    stop(parsed[char_no][["line_no"]], " contains parenthesis that does not close.")
    
  }
}
