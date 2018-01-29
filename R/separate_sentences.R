#' Put sentences on their own line
#'
#' @param filename A tex or knitr file in which to separate sentences.
#' @param hanging_footnotes (logical, default: \code{FALSE}) Should footnotes be indented?
#' @return NULL. The function is called for its side-effect: rewriting \code{filename} with separated sentences.
#' @export



separate_sentences <- function(filename, hanging_footnotes = FALSE) {
  lines <- readLines(filename)

  knitr_start <- grepl(">>=", lines, fixed = TRUE)
  knitr_stop <- grepl("^@$", lines, perl = TRUE)

  stopifnot(length(knitr_start) == length(knitr_stop))

  in_knitr <- as.logical(cumsum(knitr_start) - cumsum(knitr_stop))

  lines_with_percent <- grepl("(?<!(\\\\))%", lines, perl = TRUE)
  new_lines <- 
    if_else(in_knitr | lines_with_percent,
            lines,
            gsub(",\\footnote", ",%\n\\footnote", fixed = TRUE,
                 gsub(",\\footcite", ",%\n\\footcite", fixed = TRUE,
                      gsub(".\\footnote", ".%\n\\footnote", fixed = TRUE,
                           gsub(".\\footcite", ".%\n\\footcite", fixed = TRUE,
                                gsub("\\.\\s+([A-Z])", "\\.\n\\1", perl = TRUE,
                                     gsub("\\.[}]\\s+([A-Z])", "\\.}\n\\1", perl = TRUE, 
                                          lines)))))))
                 
                 writeLines(new_lines, filename)
  
  if (hanging_footnotes && !any(in_knitr)) {
    new_lines <- read_lines(filename)
    parsed_doc <- parse_tex(new_lines)
    footnote_extraction <- extract_mandatory_LaTeX_argument(new_lines, 
                                                            "footnote",
                                                            by.line = TRUE,
                                                            parsed_doc = parsed_doc)
    
    footnote_lines <- footnote_extraction[["line_no_min"]]
    new_lines[footnote_lines] <- paste0("\t", new_lines[footnote_lines])
  }
  writeLines(new_lines, filename)
}
