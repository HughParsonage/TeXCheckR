
combine_lines <- function(lines) {
  # If a brace is followed by a newline and a % we can proceed
  lines[endsWith(shift(lines, fill = ""), "}") & startsWith(lines, "%")] <- " "
  lines[endsWith(shift(lines, n = 2L, fill = ""), "}") & startsWith(lines, "%")] <- " "
  
  oneline <- paste0(lines, collapse = "NEW.")
  out <- strsplit(oneline, split = "NEW.NEW.NEW.", fixed = TRUE)[[1]]
  out <- gsub("NEW.", "", out, fixed = TRUE)
  out
}


