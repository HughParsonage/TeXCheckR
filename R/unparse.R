unparse <- function(parsed) {
  # Note that if all(nzchar(text)), an extra line is added
  char <- NULL
  out_text <- parsed[, .(text = paste0(char, collapse = "")), keyby = "line_no"]
  # Fill in blank lines
  out <- character(.subset2(out_text, "line_no")[nrow(out_text)] + 1L) # +1 for trailing n
  out[.subset2(out_text, "line_no")] <- .subset2(out_text, "text")
  out
}
