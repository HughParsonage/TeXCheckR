
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

stri_count_fixed_no_stringi <- function(str, pattern) {
  relevant_line_nos <- grep(pattern, str, fixed = TRUE)
  relevant_lines <- str[relevant_line_nos]
  count_on_relevant <- 
    if (nchar(pattern) == 1L) {
      split_lines <- strsplit(relevant_lines, split = "", fixed = TRUE)
      vapply(split_lines, function(x) sum(x == pattern), integer(1L))
    } else {
      # If you wanted speed, you should have used stringi!
      vapply(relevant_lines, function(line) {
        count <- 0L
        # How many times do we have to cut 'pattern' away?
        while (grepl(pattern, line, fixed = TRUE)) {
          count <- count + 1L
          # Bear in mind 'aaaaa' where pattern = 'aa'
          line <- sub(pattern, replacement = "", line, fixed = TRUE)
        }
        count
      }, 
      FUN.VALUE = integer(1L))
    }
  out <- integer(length(str))
  out[relevant_line_nos] <- count_on_relevant
  out
}

stri_locate_first_fixed <- function(str, pattern, ...) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_locate_first_fixed(str = str, pattern = pattern, ...)
  } else {
    stri_locate_first_fixed_no_stringi(str, pattern)
  }
}

stri_count_fixed <- function(str, pattern, ...) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_count_fixed(str = str, pattern = pattern, ...)
  } else {
    stri_count_fixed_no_stringi(str = str, pattern = pattern)
  }
}




