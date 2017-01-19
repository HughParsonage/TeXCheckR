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
  
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
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
    if (length(split_line_after_footnote) > footnote_closes_at && split_line_after_footnote[footnote_closes_at + 1] %in% c(".", ",")){
      cat(paste0(split_line_after_footnote,
                 collapse = ""),
          "\n")
      stop("Full stop after footnotemark.")
    }
  }
  
  
  
  cat("\u2014  No full stops after footnotemarks", "\n")
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

    if (split_line_after_footnote[footnote_closes_at - 1] != "."){
      # OK if full stop is before parenthesis.
      if (not(AND(split_line_after_footnote[footnote_closes_at - 1] == ")",
                  split_line_after_footnote[footnote_closes_at - 2] == "."))){
        cat(paste0(split_line_after_footnote,
                   collapse = ""),
            "\n")
        stop("Footnote does not end with full stop.")
      }
    }
  }
  cat("\u2014  All footnotes end with a full stop.", "\n")
  
  for (line_no in seq_along(lines[-1])){
    x <- lines[[line_no + 1L]]
    w <- lines[[line_no]]
    if (OR(AND(grepl(" \\footnote", x, fixed = TRUE),
               !grepl("\\s*\\\\footnote", x, perl = TRUE)),
           # footnote on new line without protective %
           AND(grepl("^\\s*\\\\footnote", x, perl = TRUE),
               !grepl("(?<! )%$", w, perl = TRUE)))){
      .report_error(line_no = line_no,
                    context = x,
                    error_message = "Space inserted before \\footnote")
      stop("Space inserted before footnote.")
    }
  }
  cat("\u2014  No space before footnote marks", "\n")
  invisible(NULL)
}


