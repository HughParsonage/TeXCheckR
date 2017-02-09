
remove_valid_contractions <- function(lines){
  for (valid in valid_English_contractions){
    lines <- gsub(sprintf("\\b%s\\b", valid), "<contraction>", lines, perl = TRUE, ignore.case = TRUE)
  }
  lines
}
