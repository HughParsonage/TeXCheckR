
combine_bib_fields <- function(bib) {
  bib <- stri_trim_both(bib)
  
  cumsum_brace <- function(x) {
    if (length(x)) {
      cumsum(x == "{") - cumsum(x == "}")
    } else {
      0L
    }
  }
  
  split_lines <- strsplit(bib, split = "", fixed = TRUE)
  intraentry_groups <- lapply(split_lines, cumsum_brace)
  intraentry_groups_lengths <- nchar(bib)
  for (j in seq_along(intraentry_groups)) {
    if (j > 1) {
      intraentry_groups[[j]] <- intraentry_groups[[j]] + last(intraentry_groups[[j - 1]])
    }
  }
  rm(j)
  # i is the maximum number of lines over which
  # field could be spread
  i <- 0
  while (i < 1000 && any(vapply(intraentry_groups, last, integer(1)) > 1)) {
    i + 1
    
    if (i == 1000) {
      warning("Iterated to combined bib files 1000 times. ",
              "That is, a field in your .bib file is spread over at least 1000 lines. ",
              "This is likely a bug, so please report.")
    }
    
    for (j in rev(seq_along(bib))) {
      if (intraentry_groups[[j]][1] > intraentry_groups[[j]][intraentry_groups_lengths[j]]) {
        bib[j - 1] <- paste(bib[j - 1], bib[j])
        bib <- bib[-j]
      }
    }
    split_lines <- strsplit(bib, split = "", fixed = TRUE)
    intraentry_groups <- lapply(split_lines, cumsum_brace)
    intraentry_groups_lengths <- vapply(intraentry_groups, length, integer(1))
    nonzero <- as.logical(vapply(seq_along(intraentry_groups),
                                 function(j) intraentry_groups[[j]][intraentry_groups_lengths[j]],
                                 integer(1)))
    for (J in seq_along(intraentry_groups)) {
      if (J > 1 && nonzero[J]) {
        intraentry_groups[[J]] <- intraentry_groups[[J]] + intraentry_groups[[J - 1]][intraentry_groups_lengths[J - 1]]
      }
    }
  }
  invisible(bib)
  
}


