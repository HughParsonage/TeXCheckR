#' Put sentences on their own line
#'
#' @importFrom dplyr if_else
#' @param filename A tex or knitr file in which to separate sentences.
#' @return NULL. The function is called for its side-effect: rewriting \code{filename} with separated sentences.
#' @export



separate_sentences <- function(filename){
  lines <- readLines(filename)

  knitr_start <- grepl(">>=", lines, fixed = TRUE)
  knitr_stop <- grepl("^@$", lines, perl = TRUE)

  stopifnot(length(knitr_start) == length(knitr_stop))

  in_knitr <- as.logical(cumsum(knitr_start) - cumsum(knitr_stop))

  lines_with_percent <- grepl("(?<!(\\\\))%", lines, perl = TRUE)

  if_else(in_knitr | lines_with_percent,
          lines,
          gsub(".\\footnote", ".%\n\\footnote", fixed = TRUE,
               gsub(".\\footcite", ".%\n\\footcite", fixed = TRUE,
                    gsub("\\.\\s+([A-Z])", "\\.\n\\1", lines, perl = TRUE)))) %>%
    writeLines(filename)
}
