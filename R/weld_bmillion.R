#' Unbreaking spaces between billion and million
#'
#' @param filename A LaTeX or knitr file.
#' @return NULL. This function is called for its side-effect: rewriting \code{filename} with \code{30 million} changed to \code{30~million}.
#' @importFrom magrittr %>%
#' @export

weld_bmillion <- function(filename, outfile = filename){
  readLines(filename) %>%
    gsub("([0-9]) ([bm]illion)", "\\1~\\2", x = ., perl = TRUE) %>%
    writeLines(outfile)
}
