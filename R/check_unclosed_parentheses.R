#' Check mismatched parentheses
#' @noRd

check_unclosed_parentheses <- function(filename,
                                       rstudio = FALSE,
                                       tex_lines = NULL,
                                       verbose = FALSE) {
  if (is.null(tex_lines)) {
    tex_lines <- read_lines(filename)
  }
  tex_lines <- strip_comments(tex_lines)
  
  # equations like \(x^2 + y^2 = h^2\)
  tex_lines <- gsub("\\\\\\(|\\\\\\)", "\\$", tex_lines, perl = TRUE)
  
  tex_lines[grep("\\begin{enumerate}[", tex_lines, fixed = TRUE)] <- ""
  
  
  
  if (sum(stri_count_fixed(tex_lines, pattern = "(")) != 
      sum(stri_count_fixed(tex_lines, pattern = ")"))) {
    tex_lines_split <- strsplit(tex_lines, "", fixed = TRUE)
    
    # This needs to be after all modifications to tex_lines
    # otherwise the data.table() later might warn about 
    # different lengths.
    tex_lines_nchar <- nchar(tex_lines)
    
    tex_line_chars <- unlist(tex_lines_split)
    
    final_parentheses <- exclude_lists(tex_line_chars)
    
    line_no <- NULL
    parsed <- 
      data.table(char = tex_line_chars,
                 char_no = seq_along(tex_line_chars),
                 line_no = rep(seq_along(tex_lines), times = tex_lines_nchar),
                 parenthesis_group = final_parentheses)
    final_paren_group <- final_parentheses[length(final_parentheses)]
    if (final_paren_group > 0L) {
      # Opens
      
      char_no <- max(which(and(final_parentheses == last(final_parentheses),
                               final_parentheses > shift(final_parentheses, fill = 0L))))
      stop(parsed[char_no][["line_no"]], " contains parenthesis that does not close.")
    } else if (final_paren_group < 0L) {
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

exclude_lists <- function(tex_chars, depth = 0L, prefix_type = c("1", "a", "A")) {
  # cat(tex_chars, sep = "")
  # cat("\n")
  final_parentheses <- cumsum(tex_chars == "(") - cumsum(tex_chars == ")")
  
  if (min(final_parentheses) < 0L) {
    # Numbers like 1) foo 2) bar 3) baz are ok
    # As are a) b) c) but only length-1 letters TODO: determine list
    first_unclosed <- min(which(final_parentheses < 0L))
    
    # final_parentheses/
    # Letter or num/   -1 -2 -3 -4
    #                   1  2  3  4
    if (depth == 0L) {
      if (tex_chars[first_unclosed - 1L] == "1") {
        tex_chars[first_unclosed] <- ">"
        return(exclude_lists(tex_chars, depth = depth + 1L, prefix_type = "1"))
      } else if (tex_chars[first_unclosed - 1L] == "a") {
        tex_chars[first_unclosed] <- ">"
        return(exclude_lists(tex_chars, depth = depth + 1L, prefix_type = "a"))
      } else if (tex_chars[first_unclosed - 1L] == "A") {
        tex_chars[first_unclosed] <- ">"
        return(exclude_lists(tex_chars, depth = depth + 1L, prefix_type = "A"))
      } else {
        return(final_parentheses)
      }
      
    } else {
      permitted_prefix <- switch(prefix_type, 
                                 "1" = as.character(depth + 1L),
                                 "a" = letters[depth + 1L],
                                 "A" = LETTERS[depth + 1L])
      if (tex_chars[first_unclosed - 1L] == permitted_prefix) {
        tex_chars[first_unclosed] <- ">"
        return(exclude_lists(tex_chars,
                             depth = depth + 1L,
                             prefix_type = prefix_type))
      } else if (tex_chars[first_unclosed - 1L] %chin% c("1", "a", "A")) {
        return(exclude_lists(tex_chars, depth = 0L))
      } else {
        return(final_parentheses)
      }
    }
  }
  final_parentheses
}



