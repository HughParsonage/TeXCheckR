#' Check biber
#'
#' @param path Containing the bst file.
#' @export

check_biber <- function(path){
  blg.file <- dir(path = path, pattern = "blg$", full.names = TRUE)
  if (length(blg.file) > 1L){
    stop("More than one blg file.")
  } else {
    if (length(blg.file) == 1L){
      blg <- readLines(blg.file, warn = FALSE)
      # Remove legislation marks:
      blg <- blg[!grepl("WARN - year field .((Cth)|(NSW)|(Vic)|(SA)|(Tas)|(Qld)|(WA)|(ACT)|(NT)). in entry .*(Act)", blg, perl = TRUE)]

      if (any(grepl("WARN", blg, fixed = TRUE))){
        first_bad_entry <-
          blg[grepl("WARN", blg, fixed = TRUE)] %>%
          head(1)

        cat(sub("^.*WARN", "WARN", first_bad_entry))
        stop("Biber emitted a warning.")
      }
    }
  }
}
