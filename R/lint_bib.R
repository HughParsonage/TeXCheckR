#' Tidy bibliography so equals signs align
#' @param bib_file The bib file to tidy.
#' @param outfile Optionally, the tidied bib file to write to.
#' @param leading_spaces The number of spaces before each field within an entry.
#' @details Aligns the equals signs in \code{bib_file} and puts a constant
#' @export

lint_bib <- function(bib_file, outfile = bib_file, leading_spaces = 2L){
  stopifnot(length(bib_file) == 1L, grepl("\\.bib$", bib_file, perl = TRUE))

  bib <- readLines(bib_file, encoding = "UTF-8", warn = FALSE)

  is_field <- grepl("=", bib, fixed = TRUE)
  field_width <- nchar(trimws(gsub("[=].*$", "", bib, perl = TRUE)))

  widest_field <- max(field_width[is_field])

  out <- bib

  # Vectorized gsub:
  for (line in seq_along(bib)){
    # Replace every field line with
    # two spaces + field name + spaces required for widest field + space
    if (is_field[line]){
    spaces_req <- widest_field - field_width[line]
    out[line] <-
      gsub("^\\s*(\\w+)\\s*[=]\\s*\\{",
           paste0(paste0(rep(" ", leading_spaces), collapse = ""),
                  "\\L\\1",
                  paste0(rep(" ", spaces_req), collapse = ""),
                  " = {"),
           bib[line],
           perl = TRUE)
    }
  }
  
  # Add commas: 
  out[is_field] <- gsub("\\}$", "\\},", out[is_field], perl = TRUE)
  
  writeLines(out, outfile, useBytes = TRUE)
}
