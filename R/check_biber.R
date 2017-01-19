#' Check biber
#'
#' @param path Containing the blg file.
#' @export

check_biber <- function(path = "."){
  blg.file <- dir(path = path, pattern = "blg$", full.names = TRUE)
  if (length(blg.file) > 1L){
    stop("More than one blg file.")
  } else {
    if (length(blg.file) == 1L){
      blg <- readLines(blg.file, warn = FALSE)
      
      biber_version <-
        gsub("^.*This is Biber ([0-9]+\\.[0-9]).*",
             "\\1",
             grep("This is Biber",
                  blg,
                  fixed = TRUE,
                  value = TRUE)[[1]],
             perl = TRUE)
      
      if (length(biber_version) == 0L){
        stop("Biber version could not be detected")
      }
      
      if (biber_version < "2.6"){
        stop("biber version is ", biber_version, " but >=2.6 is required.")
      }
    
      # Remove legislation marks:
      blg <- blg[!grepl("WARN - year field .((Cth)|(NSW)|(Vic)|(SA)|(Tas)|(Qld)|(WA)|(ACT)|(NT)|(NZ)). in entry .*((Act)|(Reg)|(Bill))", blg, perl = TRUE)]

      # WARN not WARNINGS
      if (any(grepl("WARN ", blg, fixed = TRUE))){
        first_bad_entry <-
          blg[grepl("WARN ", blg, fixed = TRUE)] %>%
          .[1]

        cat(sub("^.*WARN ", "WARN", first_bad_entry))
        stop("Biber emitted a warning.")
      }
    }
  }
  invisible(NULL)
}
