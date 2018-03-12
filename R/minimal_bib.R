#' Generate a minimal bibliography file
#' @param path A directory containing a document after it has been run with \code{pdflatex}.
#' @param bbl.file A \code{.bbl} file.
#' @param bib.files The \code{.bib} file or files that were used by BibLaTeX to produce the bibliography.
#' If \code{NULL}, the default, the files are inferred from the contents of \code{\\\\addbibresource} within the (unique) \code{.tex} file are used.
#' @param out.bib The new file of bibliography.
#' @export

minimal_bib <- function(path = ".",
                        bbl.file = NULL,
                        bib.files = NULL,
                        out.bib = bib.files) {
  if (nzchar(Sys.which("bibexport"))) {
    system("bibexport -o extracted.bib myarticle.aux")
  } else {
    
    if (is.null(bbl.file)) {
      bbl.file <- dir(path = path,
                      pattern = "\\.bbl$",
                      full.names = TRUE)
      if (length(bbl.file)) {
        if (length(bbl.file) > 1L) {
          stop("`path` contained more than one .bbl file, yet `bbl.file = NULL`. ",
               "Either ensure `path` contains only one .bbl file or provide ", 
               "`bbl.file` explicitly.")
        }
      } else {
        stop("`path` did not contain a .bbl file. ",
             "Either ensure `path` contains only one .bbl file or provide ", 
             "`bbl.file` explicitly.")
      }
    }
    
    bbl <- read_lines(bbl.file)
    
    if (is.null(bib.files)) {
      file.tex <- dir(path = path,
                      pattern = "\\.tex$",
                      full.names = TRUE)
      if (length(file.tex)) {
        extract_addbibresource <- function(lines) {
          addbibresources <- grep("\\addbibresource", lines, fixed = TRUE, value = TRUE)
          .subset2(extract_mandatory_LaTeX_argument(addbibresources,
                                                    "addbibresource"),
                   "extract")
        }
        
        bib.files <-
          lapply(file.tex, 
                 read_lines) %>%
          lapply(extract_addbibresource) %>%
          unlist(use.names = FALSE)
        
      } else {
        stop("`bib.file = NULL`, yet `path` does not contain a `.tex` file so cannot be inferred. ",
             "Either ensure `path` contains a .tex file, containing a line with \\addbibresource{<file.bib>}")
      }
    }
    
    keys_used <- extract_LaTeX_argument(bbl, "entry")[["extract"]]
    
    out <- 
      if (length(bib.files) == 1L) {
        bib_DT <- fread_bib(bib.files)
        
        line_nos <-
          bib_DT[key %in% keys_used] %>%
          .[["line_no"]]
        
        bib <- read_lines(bib.files)
        out <- character(length(bib))
        keep_line_nos <- 
          sort(unique(c(line_nos,
                        line_nos + 1L,
                        line_nos - 1L)))
        
        bib[keep_line_nos]
        
      } else {
        lapply(bib.files, function(bib.file) {
          bib_DT <- fread_bib(bib.file)
          
          line_nos <-
            bib_DT[key %in% keys_used] %>%
            .[["line_no"]]
          
          bib <- read_lines(bib.file)
          out <- character(length(bib))
          keep_line_nos <- 
            unique(c(line_nos,
                     line_nos + 1L,
                     line_nos - 1L))
          
          bib[keep_line_nos]
        }) %>%
          unlist
      }
    
    if (length(out.bib) > 1L) {
      warning("`out.bib` is length-", length(out.bib), ". ", 
              "Only the first file, ", out.bib[1], " will be used.")
      out.bib <- out.bib[1L]
    }
    
    if (!file.exists(out.bib)) {
      if (!dir.exists(dirname(out.bib))) {
        hutils::provide.dir(dirname(out.bib))
      }
      file.create(out.bib)
    }
  }
  write_lines(out, path = out.bib)
}


