
remove_valid_contractions <- function(lines){
  for (valid in valid_English_contractions) {
    # Note may contain false positives (xisn't) but that's ok
    # because it's only meant to limit the execution time in the for-loop
    contain_valid <- grepl(valid, lines, perl = TRUE, ignore.case = TRUE)
    ws_valid <- formatC(" ", width = nchar(valid))
    if (any(contain_valid)) {
      lines[contain_valid] <- 
        gsub(sprintf("\\b%s\\b", valid), ws_valid, lines[contain_valid], perl = TRUE, ignore.case = TRUE)
    }
  }
  lines
}
