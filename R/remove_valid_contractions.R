
remove_valid_contractions <- function(lines){
  for (valid in valid_English_contractions){
    lines <- gsub(valid, "<contraction>", lines, fixed = TRUE)
  }
  lines
}
