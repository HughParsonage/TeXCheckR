
read_lines <- function(...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_lines(...)
  } else {
    readLines(...)
  }
}

write_lines <- function(...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_lines(...)
  } else {
    writeLines(...)
  }
}

