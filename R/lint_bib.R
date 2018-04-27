#' Tidy bibliography so equals signs align
#' @param bib_file The bib file to tidy.
#' @param outfile Optionally, the tidied bib file to write to.
#' @param leading_spaces The number of spaces before each field within an entry.
#' @details Aligns the equals signs in \code{bib_file} and ensures all fields have a trailing comma.
#' @export

lint_bib <- function(bib_file, outfile = bib_file, leading_spaces = 2L){
  
  stopifnot(length(bib_file) == 1L, grepl("\\.bib$", bib_file, perl = TRUE))

  bib <- 
    readLines(bib_file, encoding = "UTF-8", warn = FALSE) %>%
    trimws
  # Correct fields
  # coalesce journal --> journaltitle 
  bib <- gsub("^journaltitle\\s*[=]", "  journal =", bib, perl = TRUE)
  # Remove things like type = {report}, which are redundant
  bib <- bib[!grepl("^type\\s*[=]", bib, perl = TRUE)]
  
  # Fields like date   = 2006 
  # i.e. no braces
  bib <- gsub("[=]\\s*([^{},]+),?$", "= {\\1},", bib, perl = TRUE)
  
  # Now ensure that all fields occupy a single line:
  bib <- combine_bib_fields(bib)
  
  is_field <- grepl("=", bib, fixed = TRUE)
  field_width <- nchar(trimws(gsub("[=].*$", "", bib, perl = TRUE)))
  
  widest_field <- max(field_width[is_field])

  out <- bib
  
  # Lint things like {https://docs.education.gov.au/system/files/doc/other/2017{\_}allocation{\_}of{\_}units{\_}of{\_}study{\_}v2.pdf} in URLs
  # 'is url'
  iu <- startsWith(out, "url")
  # braced versions first so we capture both
  out[iu] <- gsub("{\\_}", "_", out[iu], fixed = TRUE)
  out[iu] <- gsub("{\\%}", "%", out[iu], fixed = TRUE)
  out[iu] <- gsub("\\_", "_", out[iu], fixed = TRUE)
  out[iu] <- gsub("\\%", "%", out[iu], fixed = TRUE)

  # Vectorized gsub. Required because spaces_required is 
  # different every line.
  for (line in seq_along(bib)) {
    # Replace every field line with
    # two spaces + field name + spaces required for widest field + space
    if (is_field[line]) {
      
      if (grepl("^\\s*(\\w+)\\s*[=]\\s*\\{", bib[line], perl = TRUE)) {
        spaces_req <- widest_field - field_width[line]
        out[line] <-
          gsub("^\\s*(\\w+)\\s*[=]\\s*\\{",
               paste0(paste0(rep(" ", leading_spaces), collapse = ""),
                      "\\L\\1",
                      paste0(rep(" ", spaces_req), collapse = ""),
                      " = {"),
               out[line],
               perl = TRUE)
      }
    }
  }
  
  # Add commas: 
  out[is_field & !startsWith(tolower(out), "@string")] <-
    sub("\\}$",
        "\\},",
        out[is_field & !startsWith(tolower(out), "@string")],
        perl = TRUE)
  
  writeLines(out, outfile, useBytes = TRUE)
}
