

check_cite_pagerefs <- function(filename){
  lines <- readLines(filename)

  lines_with_cites <- grep("cite[]", lines, fixed = TRUE, value = TRUE)

  for (line in lines_with_cites){
    if (grepl("cite\\[\\]\\[[0-9]+-[0-9]+\\]", line, perl = TRUE)){
      cat(line)
      stop("Page reference appears wrong.")
    }
  }
}
