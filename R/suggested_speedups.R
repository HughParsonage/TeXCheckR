
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

stri_locate_first_fixed_no_stringi <- function(str, pattern) {
  out <- matrix(NA_integer_, nrow = length(str), ncol = 2L)
  rows_with_pattern <- grep(pattern, str, fixed = TRUE)
  nchar_pattern <- nchar(pattern)
  for (i in rows_with_pattern) {
    res <- as.vector(regexpr(pattern = pattern,
                             text = str[i],
                             fixed = TRUE),
                     mode = "integer")
    out[i, 1L] <- res
    
  }
  out[, 2L] <- out[, 1L] + nchar_pattern - 1L
  # Conformance with stringi
  setattr(out, "dimnames", value = list(NULL, c("start", "end")))
  out
}

stri_locate_first_fixed <- function(str, pattern, ...) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_locate_first_fixed(str = str, pattern = pattern, ...)
  } else {
    stri_locate_first_fixed_no_stringi(str, pattern)
  }
}




