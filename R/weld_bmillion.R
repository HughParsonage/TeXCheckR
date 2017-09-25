#' Unbreaking spaces between billion and million
#'
#' @param filename A LaTeX or knitr file.
#' @param outfile The file to write to, defaults to \code{filename}.
#' @return NULL. This function is called for its side-effect: rewriting \code{filename} with \code{30 million} changed to \code{30~million}.
#' @export

weld_bmillion <- function(filename, outfile = filename){
  stopifnot(length(filename) == 1L)

  x <- readLines(filename)
  if_else(isR_line_in_knitr(x) | grepl("(?<!(\\\\))%", x, perl = TRUE),
          x,
          gsub("([0-9]) ([bm]illion)", "\\1~\\2", x = x, perl = TRUE)) %>%
    writeLines(outfile)
}
