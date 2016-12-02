#' Check repetitive cross-references
#' @param filename A LaTeX file
#' @return NULL

check_repetitive_xrefs <- function(filename){
  lines <- readLines(filename)

  lines_with_VrefCref <- grep("[CVcv]ref", lines, perl = TRUE)

  for (line in lines_with_VrefCref){
    # 'See Figure \Vref{fig:'
    if (grepl("((figure)|(table)|(box)) .[CVcv]ref", line, perl = TRUE, ignore.case = TRUE)){
      cat(line)
      stop("Line has repetitive ref. May appear as e.g. 'See Figure Figure 4.2.'")
    }
  }
}
