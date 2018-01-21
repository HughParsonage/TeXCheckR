
minimal_bib <- function(bbl.file, bib.file, out.bib) {
  bbl <- read_lines(bbl.file)
  keys_used <- extract_LaTeX_argument(bbl, "entry")[["extract"]]
  bib_DT <- fread_bib(out.bib)
  line_nos <-
    bib_DT[key %in% keys_used] %>%
    .[["line_no"]]
  
  bib <- read_lines(bib.file)
  out <- character(length(bib))
  keep_line_nos <- 
    unique(c(line_nos,
             line_nos + 1L,
             line_nos - 1L))
  
  out[keep_line_nos] <- bib[keep_line_nos]
  
  write_lines(out,
              path = out.bib)
}


