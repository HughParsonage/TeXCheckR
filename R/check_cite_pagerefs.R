

check_cite_pagerefs <- function(filename){
  lines <- readLines(filename, warn = FALSE)

  lines_with_cites <- lines[grepl("cite[", lines, fixed = TRUE) | grepl("cite{", lines, fixed = TRUE)]

  for (line in lines_with_cites){
    if (grepl("cite\\[[^\\]]+\\][{]", line, perl = TRUE)){
      stop("Use postnote for pagerefs.")
    }

    # Check constructions like p. 93
    if (grepl("cite\\[\\]\\[p\\.?\\s*[0-9]+\\]", line, perl = TRUE)){
      cat(line)
      stop("Unnecessary p in postnote.")
    }
    # Check single hyphen between pagerefs
    if (grepl("cite\\[\\]\\[[0-9]+-[0-9]+\\]", line, perl = TRUE)){
      cat(line)
      stop("Page reference appears wrong.")
    }
  }
  rm(line)
  invisible(NULL)
}
