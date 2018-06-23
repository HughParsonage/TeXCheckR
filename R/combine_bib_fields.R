
combine_bib_fields <- function(bib, verbose = getOption("TeXCheckR.verbose", FALSE)) {
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
    if (j > 1L) {
      intraentry_groups[[j]] <- intraentry_groups[[j]] + last(intraentry_groups[[j - 1L]])
    }
  }
  rm(j)
  # i is the maximum number of lines over which
  # field could be spread
  i <- 0L
  while (i < 100L && max(vapply(intraentry_groups, last, 0L)) > 1L) {
    i <- i + 1L
    if (verbose && {i %% 10L} == 0L) {
      cat(i, sep = "\n")
    }
    
    if (i == 100L) {
      warning("Iterated to combined bib files 100 times. ",
              "That is, a field in your .bib file is spread over at least 1000 lines. ",
              "This is likely a bug, so please report.")
    }
    
    for (j in rev(seq_along(bib))) {
      last_group <- intraentry_groups[[j]][intraentry_groups_lengths[j]]
      if (j > 1L) {
        prev_last_group <- intraentry_groups[[j - 1L]][intraentry_groups_lengths[j - 1L]]
      }
      first_group <- intraentry_groups[[j]][1]
      
      if (OR(AND(intraentry_groups_lengths[j] > 0L,
                 first_group > last_group),
             # If a brace is pushed on to the next line, the initial group won't be seen
             prev_last_group > 1L && trimws(bib[j]) == "},")) {
        if (verbose) {
          cat("bib[j - 1]:", bib[j - 1L], "==> \t", 
              paste(bib[j - 1L], bib[j]), "\n")
        }
        bib[j - 1L] <- paste(bib[j - 1L], bib[j])
        bib <- bib[-j]
      } 
    }
    split_lines <- strsplit(bib, split = "", fixed = TRUE)
    intraentry_groups <- lapply(split_lines, cumsum_brace)
    intraentry_groups_lengths <- vapply(intraentry_groups, length, 0L)
    nonzero <- as.logical(vapply(seq_along(intraentry_groups),
                                 function(j) intraentry_groups[[j]][intraentry_groups_lengths[j]],
                                 0L))
    for (J in seq_along(intraentry_groups)) {
      if (J > 1L) {
        intraentry_groups[[J]] <-
          intraentry_groups[[J]] +
          intraentry_groups[[J - 1L]][intraentry_groups_lengths[J - 1L]]
      }
    }
  }
  invisible(bib)
  
}


