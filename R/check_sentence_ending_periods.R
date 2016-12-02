

check_sentence_ending_periods <- function(filename){
  lines <- readLines(filename)

  lines2 <- lines[isR_line_in_knitr(lines)]

  for (line in lines2){
    if (grepl("[A-Z]\\.\\s+[A-Z]", line, perl = TRUE)){
      cat(line)
      stop("Sentence ending period?")
    }
  }
}
