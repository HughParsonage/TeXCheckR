
combine_lines <- function(lines){
  oneline <- paste0(lines, collapse = "NEW.")
  out <- strsplit(oneline, split = "NEW.NEW.NEW.", fixed = TRUE)[[1]]
  out <- gsub("NEW.", "", out, fixed = TRUE)
  out
}


