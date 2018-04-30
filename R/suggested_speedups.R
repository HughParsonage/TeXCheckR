
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

stri_locate_all_regex_no_stri <- function(str, pattern) {
  row_has_pattern <- grepl(pattern, str, perl = TRUE)
  ans <- 
    lapply(seq_along(str), function(i) {
      if (row_has_pattern[i]) {
        gregexprs <- 
          gregexpr(pattern = pattern, 
                   text = str[i], 
                   perl = TRUE)
        nchar_pattern <- attr(gregexprs[[1L]], "match.length")[1L]
        out <- matrix(NA_integer_, nrow = length(gregexprs[[1L]]), ncol = 2L)
        for (j in seq_along(gregexprs[[1L]])) {
          out[j, 1L] <- gregexprs[[1L]][j]
        }
        out[, 2L] <- out[, 1L] + nchar_pattern - 1L
        # Conformance with stringi
      } else {
        out <- matrix(NA_integer_, nrow = 1L, ncol = 2L)
      }
      setattr(out, "dimnames", value = list(NULL, c("start", "end")))
      out
    })
  ans
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

stri_sub_no_stringi <- function(str, from, to) {
  out <- str
  nchar_out <- nchar(out) + 1L
  stopifnot(length(from) == 1L, length(to) == 1L)
  FROM <- rep_len(from, length(out))
  TO <- rep_len(to, length(out))
  if (from < 0L) {
    FROM <- nchar_out + from
  } else {
    FROM <- rep_len(from, length(out))
  }
  if (to < 0L) {
    TO <- nchar_out + to
  } else {
    TO <- rep_len(to, length(out))
  }
  substr(out, FROM, TO)
}

stri_sub <- function(str, from = 1L, to = -1L, .len) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    if (missing(.len)) {
      stringi::stri_sub(str = str, from = from, to = to)
    } else {
      stringi::stri_sub(str = str, from = from, to = to, length = .len)
    }
  } else {
    stri_sub_no_stringi(str = str, from = from, to = to)
  }
}


stri_trim_both <- function(str) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trim_both(str)
  } else {
    trimws(str)
  }
}







