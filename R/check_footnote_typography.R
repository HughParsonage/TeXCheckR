#' Check footnote typography
#' @param filename A LaTeX file.
#' @return Called for its side-effect.
#' @export

check_footnote_typography <- function(filename, ignore.lines = NULL){
  lines <- readLines(filename)

  # To avoid footnotesize
  lines <- gsub("footnotesize", "FOOTNOTESIZE", lines, fixed = TRUE)
  lines <- lines[!grepl("GenericWarning", lines, fixed = TRUE)]

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
  message("No full stops after footnotemarks")
  rm(line)

  for (line in lines_with_footnote){
    footnote_closes_at <- position_of_closing_brace(line = line, prefix = "footnote")
    split_line_after_footnote <- strsplit(gsub("^.*footnote", "", line, perl = TRUE), split = "")[[1]]

    if (length(split_line_after_footnote[footnote_closes_at - 1] != ".") == 0){
      cat(paste0(split_line_after_footnote,
                 collapse = ""),
          "\n")
      stop("Argument length 0")
    }

    if (split_line_after_footnote[footnote_closes_at - 1] %notin% c(".", ".)")){
      cat(paste0(split_line_after_footnote,
                 collapse = ""),
          "\n")
      stop("Footnote does not end with full stop.")
    }
  }

  for (line in lines){
    if (grepl(" \\footnote", line, fixed = TRUE)){
      cat(line)
      stop("Space before footnote.")
    }
  }
  message("No space before footnote marks")
  rm(line)
  NULL
}


# test_that("Detected", {
#   expect_error(check_footnote_typography("tests/footnote-bad-full-stop.rnw"), regexp = "Full stop after footnotemark")
# })
