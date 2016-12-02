

scan_hyphenation <- function(file.pdf){
  stopifnot(grepl("pdf$", file.pdf))
  system(paste0("pdftotext -layout ", file.pdf))
  on.exit(file.remove(gsub("pdf$", "txt", file.pdf)))
  pdf_as_text <- readLines(gsub("pdf$", "txt", file.pdf))

  paraz <- list()
  paragraph <- 1
  n_hyphens <- 0
  data.table::rbindlist(lapply(seq_along(pdf_as_text), function(l){
    if (pdf_as_text[[l]] == ""){
      paraz[[paragraph]] <- list(n_hyphens = n_hyphens, context = pdf_as_text[[l - 1]], ell = l)
      paragraph <- paragraph + 1
      n_hyphens <- 0L
    } else {
      if (grepl("-$", pdf_as_text[[l]])){
        n_hyphens <- n_hyphens + 1L
      }
    }
    paraz
  }))
}
