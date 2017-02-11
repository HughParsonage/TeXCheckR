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
  
  # Check overview for footnotes
  overview_start <- lines == "\\begin{overview}"
  overview_end <- lines == "\\end{overview}"
  if (any(overview_start)){
    is_overview <- as.logical(cumsum(overview_start) - cumsum(overview_end))
    if (any(grepl("\\footnote", lines[is_overview], fixed = TRUE))){
      if (!any(overview_end)){
        stop("Emergency stop: No \\end{overview} found in document. Check your LaTeX syntax and try again")
      } else {
        line_no <- grep("\\footnote", lines[is_overview], fixed = TRUE)
        .report_error(line_no = line_no,
                      context = lines[line_no], 
                      error_message = "Footnote detected in overview.")
        stop("Footnote detected in overview. Remove any footnotes in the overview.")
      }
    }
  }

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
    grepl("\\\\footcite(?!s)", lines, perl = TRUE)
  
  line_nos_with_footcites <-
    grepl("\\footcites", lines, fixed = TRUE)
  
  # Have to separate otherwise the replacement occurs on the wrong command name
  if (any(line_nos_with_footcite)){
    lines_with_footcite <- lines[line_nos_with_footcite] 
    
    lines_with_footcite_noarg <- lines_with_footcite
    lines_with_footcite_noarg <- replace_nth_LaTeX_argument(lines_with_footcite_noarg, 
                                                            command_name = "footcite",
                                                            n = 1L,
                                                            replacement = "")
    
    chars_after_footcite <- gsub("^.*\\\\footcite\\{\\}\\s*(.)?.*$", 
                                "\\1", 
                                lines_with_footcite_noarg, 
                                perl = TRUE)
    
    if (any(chars_after_footcite %in% c(".", ",", ":", ";", "'", '"', "?", "-"))){
      first_footcite_w_punct <- which(chars_after_footcite %in% c(".", ",", ":", ";", "'", '"', "?", "-"))[[1]]
      line_no <- line_nos_with_footcite[first_footcite_w_punct]
      .report_error(line_no = line_nos_with_footcite[first_footcite_w_punct],
                    context = lines[line_no], 
                    error_message = "Punctuation mark after footcite number ", first_footcite_w_punct, ".")
      stop("Punctuation mark after footcite number ", first_footcite_w_punct, ".")
    }
  }
  
  if (any(line_nos_with_footcites)){
    # We can't just gsub {[A-Za-z]} because we don't know how many braces are needed.
    lines_with_footcites <- lines[line_nos_with_footcites] 
    
    sup_braces_after_footcites <- 
      gsub("[^\\}]+", "", gsub("^.*footcite", "", lines_with_footcites)) %>%
      nchar %>%
      max
    
    lines_with_footcites_noarg <- lines_with_footcites
    
    for (n in seq_len(sup_braces_after_footcites)){
      lines_with_footcites_noarg <-
        replace_nth_LaTeX_argument(lines_with_footcites_noarg,
                                   "footcites",
                                   n = n,
                                   replacement = "",
                                   warn = FALSE)
    }
    
    # Now that footcites are just {} (repeated), find the first char thereafter.
    chars_after_footcites <- gsub("^.*\\\\footcites?(?:\\{\\})+\\s*(.)?.*$", "\\1", lines_with_footcites_noarg, perl = TRUE)
    
    if (any(chars_after_footcites %in% c(".", ",", ":", ";", "'", '"', "?"))){
      stop("Punctuation mark after \\footcites number ",
           which(chars_after_footcites %in% c(".", ",", ":", ";", "'", '"', "?")[[1]]))
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
      .report_error(error_message = "Couldn't determine where footnotes closed.",
                    context = paste0(split_line_after_footnote,
                                     "\n",
                                     collapse = ""),
                    advice = "Examine this line for multiple paragraphs or unclosed footnotes.")
      stop("Argument length 0. You may want to consider ignoring this line.")
    }

    if (split_line_after_footnote[footnote_closes_at - 1] %notin% c(".", "?")){
      # OK if full stop is before parenthesis.
      if (not(OR(AND(split_line_after_footnote[footnote_closes_at - 1] == ")",
                     split_line_after_footnote[footnote_closes_at - 2] %in% c(".", "?", "'")),
                 AND(split_line_after_footnote[footnote_closes_at - 1] == "}",
                     split_line_after_footnote[footnote_closes_at - 2] %in% c(".", "?", "'"))))){
        .report_error(context = paste0("\n\\footnote\n         ",
                                       paste0(split_line_after_footnote[1:footnote_closes_at],
                                              collapse = ""),
                                       "\n"), 
                      error_message = "Footnote does not end with full stop.")
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


