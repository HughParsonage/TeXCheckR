#' Check footnote typography
#' @param filename A LaTeX file.
#' @param ignore.lines Lines to ignore (for example, those using the word 'footnote').
#' @param .report_error A function to provide context to any errors.
#' @return Called for its side-effect.
#' @details This function when applied to a LaTeX file will throw an error if: (1) footnotes
#' @export

check_footnote_typography <- function(filename, ignore.lines = NULL, .report_error){
  
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  lines <- read_lines(filename)
  if (!is.null(ignore.lines)){
    lines <- lines[-ignore.lines]
  }

  # Remove commentaries (but not the comment symbol)
  # Need to include percentage signs though

  lines <- gsub("((?<!(\\\\))%).*$", "%", lines, perl = TRUE)

  # To avoid footnotesize
  lines <- gsub("footnotesize", "FOOTNOTESIZE", lines, fixed = TRUE)
  lines <- lines[!grepl("GenericWarning", lines, fixed = TRUE)]
  # Don't try to parse the word 'footnote' outside a control sequence.
  # 'Sentence containing word footnote' and '\\footnotemark' shouldn't be detected.
  lines <- gsub("([^\\\\])footnote", "\\1fnote", lines)
  lines <- gsub("\\\\footnote(?![{])", "\\\\fnote\\1", lines, perl = TRUE)
  
  # More than one footnote on a line won't be good.
  if (any(grepl("\\\\foot(?:(?:note)|(?:cite)).*\\\\foot(?:(?:note)|(?:cite))", 
                lines,
                perl = TRUE))){
    lines <- read_lines(filename)
    line_no <- grep("\\\\foot(?:(?:note)|(?:cite)).*\\\\foot(?:(?:note)|(?:cite))", lines, perl = TRUE)[1]
    .report_error(line_no = line_no, 
                  context = lines[[line_no]], 
                  error_message = "\\footnote/\\footcite cannot occur twice on the same line in the source. Break this sentence up into multiple lines.")
    stop("\\footnote/\\footcite cannot occur twice on the same line in the source.")
  }

  combined_lines <- combine_lines(lines)

  lines_by_footnote <-
    combined_lines %>%
    grep("\\footnote", ., fixed = TRUE, value = TRUE) %>%
    strsplit(split = "(?=([^\\s]footnote))", perl = TRUE) %>%
    unlist

  lines_with_footnote <- grep("footnote", lines_by_footnote, fixed = TRUE, value = TRUE)

  # Check full stops
  for (line in lines_with_footnote){
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    if (is.infinite(footnote_closes_at))
      break
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]
    if (AND(length(split_line_after_footnote) > footnote_closes_at,
            split_line_after_footnote[footnote_closes_at + 1] %in% c(".", ",", ";", "?", ":", "'", '"'))){
      .report_error(context = paste0("\\footnote\n         ",
                                     paste0(split_line_after_footnote[1:(footnote_closes_at + 1)], 
                                            collapse = "")), 
                    error_message = "Punctuation after footnotemark.")
      stop("Punctuation after footnotemark.")
    }
  }
  
  line_nos_with_footcite <- 
    grepl("\\footcite", lines, fixed = TRUE)
  
  if (any(line_nos_with_footcite)){
    lines_with_footcite <- 
      lines %>%
      .[line_nos_with_footcite] %>%
      gsub(paste0("((?:footcites?)|(?:\\}))", 
                  "\\[\\]", 
                  "\\[", "[^\\]]+", "\\]\\{"), 
           "\\1\\{",
           x = .,
           perl = TRUE)
    
    sup_braces_after_footcite <- 
      gsub("[^\\}]+", "", gsub("^.*footcite", "", lines_with_footcite)) %>%
      nchar %>%
      max
    
    lines_with_footcite <- replace_nth_LaTeX_argument(lines_with_footcite, "footcite", n = 1, replacement = "")
    
    for (n in 1:max(sup_braces_after_footcite)){
      lines_with_footcite <- replace_nth_LaTeX_argument(lines_with_footcite, "footcites", n = n, replacement = "", warn = FALSE)
    }
    
    chars_after_footcite <- gsub("^.*\\\\footcites?(?:\\{\\})+\\s*(.)?.*$", "\\1", lines_with_footcite, perl = TRUE)
    
    if (any(chars_after_footcite %in% c(".", ",", ":", ";", "'", '"', "?"))){
      stop("Punctuation mark after footcite")
    }
  }
  
  for (line in lines_with_footnote){
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    if (is.infinite(footnote_closes_at))
      break
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]
    if (length(split_line_after_footnote) > footnote_closes_at && split_line_after_footnote[footnote_closes_at + 1] %in% c(".", ",")){
      cat(paste0(split_line_after_footnote,
                 collapse = ""),
          "\n")
      stop("Full stop after footnotemark.")
    }
  }
  
  
  
  # cat("\u2014  No full stops after footnotemarks", "\n")
  rm(line)

  for (line in lines_with_footnote){
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]

    if (length(split_line_after_footnote[footnote_closes_at - 1] != ".") == 0){
      cat(paste0(split_line_after_footnote,
                 collapse = ""),
          "\n")
      stop("Argument length 0. You may want to consider ignoring this line.")
    }

    if (split_line_after_footnote[footnote_closes_at - 1] %notin% c(".", "?")){
      # OK if full stop is before parenthesis.
      if (not(AND(split_line_after_footnote[footnote_closes_at - 1] == ")",
                  split_line_after_footnote[footnote_closes_at - 2] %in% c(".", "?")))){
        cat("\\footnote\n       ",
            paste0(split_line_after_footnote[1:footnote_closes_at],
                   collapse = ""),
            "\n")
        stop("Footnote does not end with full stop.")
      }
    }
  }
  # cat("\u2014  All footnotes end with a full stop.", "\n")
  
  # Space before footnote
  a1 <- grepl(" \\footnote", lines, fixed = TRUE)
  
  # Nothing but spaces before footnote is ok ...
  b1 <- grepl("^\\s*\\\\footnote", lines, perl = TRUE)
  # ... provided there is a protective % on the line before {lines[-1]}
  # and there isn't a space before that. {(?<! )}
  b2 <- shift(grepl("(?<! )%$", lines, perl = TRUE), type = "lag", fill = FALSE)
  
  # a1 b1 b2  Test  Expect  Description
  #  T  T  T  1     PASS    Tabbed footnote on own line: b2 protects
  #  T  T  F  2     FAIL    Tabbed footnote without protection.
  #  T  F  T  3     FAIL    Ordinary space (and txt) before fn % irrelevant -- protective space has no effect
  #  T  F  F  4     FAIL    Ordinary space (and txt) before fn: lazy dog \footnote
  #  F  T  T  5     PASS    Non-tabbed footnote \footnote at start of text: b2 protects
  #  F  T  F  6     FAIL    Non-tabbed footnote without protection
  #  F  F  T  7     PASS    No footnote
  #  F  F  F  8     PASS    No footnote
  
  if (any(or(a1 & !(b1 & b2), 
             b1 & !b2))){
    line_no <- which(or(a1 & !(b1 & b2), 
                        b1 & !b2))[[1]]
    context <- 
      lines[line_no] 
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Space inserted before \\footnote")
    stop("Space inserted before footnote.")
  }
  
  if (any(grepl("\\footnote{ ", lines, fixed = TRUE))){
    line_no <- grep("\\footnote{ ", lines, fixed = TRUE)[[1]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Leading spacing in footnotetext.")
    stop("Leading spacing in footnotetext.")
  }
  
  # cat("\u2014  No space before footnote marks", "\n")
  invisible(NULL)
}


