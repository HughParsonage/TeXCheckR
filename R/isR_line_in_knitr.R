#' Is a line in knitr R or not?
#'
#' @param lines Lines to check, as in the result of \code{readLines}. Not a filename.
#' @return \code{TRUE} if in knitr chunk (including boundaries). \code{FALSE} otherwise.
#' @export

isR_line_in_knitr <- function(lines){
  starts <- grepl("^<<(.*)>>=\\s*$", lines, perl = TRUE)
  stops  <- grepl("^\\s*@\\s*(%+.*|)$", lines, perl = TRUE)

  as.logical(cumsum(starts - stops))

}
