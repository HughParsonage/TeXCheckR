#' Check dashes entered as hyphens
#'
#' @param filename A tex or Rnw file.
#' @return File stops and \code{cat()}s on any line where a hyphen is surrounded by a space.
#' @export

check_dashes <- function(filename){
  lines <- readLines(filename)

  lines2 <- lines[!isR_line_in_knitr(lines)]
  for (line in lines2){
    if (grepl(" - " , line, fixed = TRUE)){
      cat(line)
      stop("Check dash as hyphen.")
    }
  }
}
