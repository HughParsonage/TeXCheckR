#' Check footnote typography
#' @param filename A LaTeX file.
#' @param ignore.lines Lines to ignore (for example, those using the word 'footnote').
#' @param .report_error A function to provide context to any errors.
#' @param rstudio (logical, default: \code{FALSE}) Should the RStudio API be used?
#' @return Called for its side-effect.
#' @details See \url{https://github.com/HughParsonage/grattex/blob/master/doc/grattexDocumentation.pdf} for full set of error conditions.
#' @examples 
#' \dontrun{
#'   tex_file <- tempfile(fileext = ".tex")
#'   cat("Footnote not ending with full stop.\\footnote{No sentence}", file = tex_file)
#'   check_footnote_typography(tex_file)
#' }
#' 
#' @export

check_footnote_typography <- function(filename, ignore.lines = NULL, .report_error,
                                      rstudio = FALSE) {
  
  if (missing(.report_error)) {
    if (rstudio) {
      .report_error <- function(...) report2console(file = filename, ..., rstudio = rstudio)
    } else {
      .report_error <- function(...) report2console(file = filename, ...)
    }
  }
  
  lines <- orig_lines <- read_lines(filename)
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
        stop("Emergency stop: No \\end{overview} found in document. Check your LaTeX syntax and try again.")
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
  if (any(grepl("GenericWarning", lines, fixed = TRUE))) {
    lines[grepl("GenericWarning", lines, fixed = TRUE)] <- ""
  }
  # Don't try to parse the word 'footnote' outside a control sequence.
  # 'Sentence containing word footnote' and '\\footnotemark' shouldn't be detected.
  # Important to keep the width of 'footnote' though: so the cursor can be correctly
  # positioned.
  lines <- gsub("([^\\\\])footnote", "\\1toofnote", lines, perl = TRUE)
  # if footnote occurs at the head of a line
  lines <- gsub("^footnote", "toofnote", lines, perl = TRUE)
  lines <- gsub("\\\\footnote(?![{])", "\\\\toofnote\\1", lines, perl = TRUE)
  # Treat double quotes as singles (for checking whether footnote ends in full stop.)
  lines <- gsub("''", "'", lines, perl = TRUE)

  # End of equation preceded by punctuation treat as punctuation
  lines <- gsub(".\\]", ".", lines, fixed = TRUE)
  
  # Don't necessarily error on \end{itemize} and friends
  lines <- gsub("\\\\end\\{((?:itemize)|(?:enumerate)|(?:description))\\}", "", lines, perl = TRUE)
  

  # More than one footnote on a line won't be good.
  if (any(grepl("\\\\foot(?:(?:note)|(?:cite)).*\\\\foot(?:(?:note)|(?:cite))", 
                lines,
                perl = TRUE))){
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

  i <- 0L
  # Check full stops
  for (line in lines_with_footnote){
    i <- i + 1L
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    if (is.infinite(footnote_closes_at))
      break
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]
    if (AND(length(split_line_after_footnote) > footnote_closes_at,
            split_line_after_footnote[footnote_closes_at + 1] %chin% punctuation)) {
      
      parsed_doc <- parse_tex(orig_lines)
      location_of_footnotes <-
        extract_mandatory_LaTeX_argument(tex_lines = NULL,
                                         parsed_doc = parsed_doc,
                                         command_name = "footnote",
                                         n = 1L,
                                         by.line = TRUE)
      
      # If a footnote is written before a dash, we can end up here, even though
      # it would be ok. The following will take much longer than the rest of this function.
      if (split_line_after_footnote[footnote_closes_at + 1] == "-") {
        char_no <- NULL
        
        # If the dash occurs after a line break, a
        # space will be inserted which is ok.
        number_of_lines <- 
          parsed_doc[char_no %in% ((location_of_footnotes[i])[["char_no_max"]] + 0:1)] %>%
          .[["line_no"]] %>%
          uniqueN
        
        if (number_of_lines > 1) {
          break
        }
      }
      
      # Take a breath
      
      error_position <-
        position_end_of_footnote(2, orig_lines, must.be.punct = TRUE)
      
      if (anyNA(error_position[["line_no"]])) {
        char <- NULL
        chars_no_max <- .subset2(location_of_footnotes, "char_no_max")
        for (ichar_no_max in chars_no_max) {
          error_position <-
            parsed_doc[char_no > ichar_no_max][nzchar(char) & grepl("\\S", char, perl = TRUE)]
          next_char <- .subset2(error_position, "char")
          if (next_char[1L] %chin% punctuation) {
            error_position <- error_position[1L]
            break
          }
        }
      }
      
      
      .report_error(line_no = error_position[["line_no"]],
                    column = error_position[["column"]] + 1L,
                    # context = paste0("\\footnote\n         ",
                    #                  paste0(split_line_after_footnote[seq_len(footnote_closes_at + 1)], 
                    #                         collapse = "")), 
                    error_message = "Punctuation after footnotemark.")
      
      stop("Punctuation after footnotemark.")
    }
  }
  rm(i)
  
  line_nos_with_footcite <- 
    grepl("\\\\footcite(?!s)", lines, perl = TRUE)
  
  line_nos_with_footcites <-
    grepl("\\footcites", lines, fixed = TRUE)
  
  # Have to separate otherwise the replacement occurs on the wrong command name
  if (any(line_nos_with_footcite)) {
    lines_with_footcite <- lines[line_nos_with_footcite] 
    
    lines_with_footcite_noarg <- lines_with_footcite
    lines_with_footcite_noarg <- gsub("\\\\footcite\\{[^\\}]*\\}", 
                                      "\\\\footcite{}", 
                                      lines_with_footcite_noarg, 
                                      perl = TRUE)
    
    chars_after_footcite <- gsub("^.*\\\\footcite\\{\\}\\s*(.)?.*$", 
                                "\\1", 
                                lines_with_footcite_noarg, 
                                perl = TRUE)
    
    if (any(chars_after_footcite %fin% punctuation)){
      first_footcite_w_punct <- which(chars_after_footcite %fin% punctuation)[[1L]]
      line_no <- line_nos_with_footcite[first_footcite_w_punct]
      .report_error(line_no = line_nos_with_footcite[first_footcite_w_punct],
                    context = lines[line_no], 
                    error_message = "Punctuation mark after footcite number ", first_footcite_w_punct, ".")
      stop("Punctuation mark after footcite number ", first_footcite_w_punct, ".")
    }
  }
  
  if (any(line_nos_with_footcites)) {
    # We can't just gsub {[A-Za-z]} because we don't know how many braces are needed.
    lines_with_footcites <- lines[line_nos_with_footcites] 
    
    footcite_regex <- 
      paste0("\\\\footcites\\{", 
             "[^\\}]*",
             "(", "\\}\\{", "[^\\}]*)+", "\\}")
    
    
    
    lines_with_footcites_noarg <- 
      lines_with_footcites %>%
      gsub(footcite_regex, "\\\\footcites{}", x = ., perl = TRUE)
    
    # Now that footcites are just {} (repeated), find the first char thereafter.
    chars_after_footcites <-
      gsub("^.*\\\\footcites?(?:\\{\\})+\\s*(.)?.*$", "\\1", lines_with_footcites_noarg, perl = TRUE)
    
    if (any(chars_after_footcites %fin% punctuation)) {
      stop("Punctuation mark after \\footcites number ",
           which(chars_after_footcites %in% c(".", ",", ":", ";", "'", '"', "?")[[1]]))
    }
  }
  
  
  
  # cat("\u2014  No full stops after footnotemarks", "\n")
  rm(line)

  for (line in lines_with_footnote){
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]

    if (length(split_line_after_footnote[footnote_closes_at - 1L] != ".") == 0L){
      .report_error(error_message = "Couldn't determine where footnotes closed.",
                    context = paste0(split_line_after_footnote,
                                     "\n",
                                     collapse = ""),
                    advice = "Examine this line for multiple paragraphs or unclosed footnotes.")
      stop("Argument length 0. You may want to consider ignoring this line.")
    }
    
    # If footnote ends with trailing ws, backtrack.
    back_from_closer <- 1L
    while (split_line_after_footnote[footnote_closes_at - back_from_closer] == " ") {
      back_from_closer <- back_from_closer + 1L
    }

    if (split_line_after_footnote[footnote_closes_at - back_from_closer] %notchin% c(".", "?")) {
      # OK if full stop is before parenthesis or quotes.
      if (NOR(AND(split_line_after_footnote[footnote_closes_at - 1L] %chin% c(")", "'"),
                  split_line_after_footnote[footnote_closes_at - 2L] %chin% c(".", "?", "'")),
              AND(split_line_after_footnote[footnote_closes_at - 1L] == "}",
                  split_line_after_footnote[footnote_closes_at - 2L] %chin% c(".", "?", "'")))) {
        
        
        # CRAN Note avoidance
        extract <- last_char <- nd_last_char <- column <- NULL
        
        error_position <-
          extract_mandatory_LaTeX_argument(orig_lines,
                                           command_name = "footnote", 
                                           by.line = FALSE) %>%
          .[!endsWith(extract, ".")] %>%
          .[, last_char := stri_sub(extract, from = -1L)] %>%
          .[, nd_last_char := stri_sub(extract, from = -2L, to = -2L)] %>%
          .[hutils::nor(and(last_char %chin% c(")", "'"),
                            nd_last_char %chin% c(".", "?", "'")),
                        and(last_char == "}",
                            nd_last_char %chin% c(".", "?", "'")))] %>%
          .[1L] %>%
          parse_tex(orig_lines)[.,
                                j = .(line_no, char_no, column, last_char),
                                on = "char_no==char_no_max"]
        
        
        
        
        .report_error(context = paste0("\n\\footnote\n         ",
                                       paste0(split_line_after_footnote[seq_len(footnote_closes_at)],
                                              collapse = ""),
                                       "\n"),
                      line_no = error_position[["line_no"]],
                      column = error_position[["column"]],
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
                        b1 & !b2))[[1L]]
    context <- 
      lines[line_no] 
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Space inserted before \\footnote")
    stop("Space inserted before footnote.")
  }
  
  if (any(grepl("\\footnote{ ", lines, fixed = TRUE))){
    line_no <- grep("\\footnote{ ", lines, fixed = TRUE)[[1L]]
    context <- lines[[line_no]]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Leading spacing in footnotetext.")
    stop("Leading spacing in footnotetext.")
  }
  
  # cat("\u2014  No space before footnote marks", "\n")
  invisible(NULL)
}

# n = relative to last character
#' @return A \code{data.table} containing columns \code{line_no} and \code{column}
#' for each character \code{n} characters after the footnote. (n = 0L will always be 
#' a closing brace.).
#' @noRd
position_end_of_footnote <- function(n = 0L, orig_lines, must.be.punct = FALSE, i) {
  n <- as.integer(n)
  
  char_no <- char <- NULL
  parsed_doc <- parse_tex(orig_lines)
  out <- extract_mandatory_LaTeX_argument(tex_lines = NULL,
                                          parsed_doc = parsed_doc,
                                          "footnote",
                                          n = 1L)
  target_char_nos <- .subset2(out, "char_no_max") + n
  
  
  if (must.be.punct) {
    error_position <- 
      parsed_doc[char_no %in% target_char_nos] %>%
      .[char %chin% punctuation] %>%
      .[1]
  } else {
    error_position <- 
      parsed_doc[char_no %in% target_char_nos] %>%
      .[1]
  } 
  error_position
}

next_char_rel_footnotecite <- function(parsed_doc, direction = -1L) {
  char <- char_no <- GROUP_ID1 <- NULL
  footnote_start_candidates <-
    parsed_doc[char == "{" & 
                 shift(char, n = 1L) == "e" &
                 shift(char, n = 2L) == "t" &
                 # Technically this will match
                 # \footnite but who cares?
                 shift(char, n = 3L) %chin% c("o", "i") &
                 shift(char, n = 4L) %chin% c("n", "c") &
                 shift(char, n = 5L) == "t" &
                 shift(char, n = 6L) == "o" &
                 shift(char, n = 7L) == "o" &
                 shift(char, n = 8L) == "f" &
                 shift(char, n = 9L) == "\\", 
               list(char_no, GROUP_ID1)]
  
  last_char_nos <-
    parsed_doc[footnote_start_candidates,
               on = "GROUP_ID1",
               nomatch=0L][,
                           .(last_char_no = max(char_no)),
                           keyby = "GROUP_ID1"]
  
  out <- character(nrow(last_char_nos))
  
  for (i in seq_along(out)) {
    out[i] <- .subset2(parsed_doc[char_no == .subset2(last_char_nos, "last_char_no")[i] + 1L * direction],
                       "char")
    j <- 1L * direction
    while (out[i] %chin% c(" ", "\t", "{")) {
      out[i] <- .subset2(parsed_doc[char_no == i + j], "char")
      j + 1L
    }
  }
  out
}





