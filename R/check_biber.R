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
      blg <- read_lines(blg.file)
      
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
      blg <- blg[!grepl("n.d.", blg, fixed = TRUE)]
      # WARN not WARNINGS
      if (any(grepl("WARN ", blg, fixed = TRUE))){
        all_bad_entries <- 
          blg[grepl("WARN ", blg, fixed = TRUE)]
        
        n_bad_entries <- length(all_bad_entries)
        cat(red(symbol$cross), ": ", "biber warnings:\n")
        for (j in seq_along(all_bad_entries)){
          first_bad_entry <-
            all_bad_entries %>%
            .[j]
          
          if (grepl("I didn't find a database entry for '.'", first_bad_entry, fixed = TRUE)){
            extra_text <- 
              paste0("Probable reason: use of \\footcites or \\textcites but omitting the last key.\n",
                     "e.g.\n\t\\textcites{Knuth}.\n  or\n\t\\textcites[][33]{Knuth}[][3].")
          } else {
            extra_text <- NULL
          }
          
          cat(crayon::red(sub("^.*WARN ", "WARN", first_bad_entry)), "\n", extra_text)
          if (j == 5){
            break
          }
        }
        if (n_bad_entries > 1){
          if (n_bad_entries > 5){
            stop("Biber emitted a warning. See the above WARN messages. (Only the first five are given; there were ", n_bad_entries, " in total.")
          } else {
            stop("Biber emitted a warning. See the above WARN messages.")
          }
        } else {
          stop("Biber emitted a warning. See the above WARN message.")
        }
      }
    }
  }
  invisible(NULL)
}
