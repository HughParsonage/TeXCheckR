#' Split report into includeable files
#' @param Report.tex File to split.
#' @param include Should \code{\\include} or \code{\\input} be used? If \code{TRUE}, the default, \code{\\include} is used.
#' @param subdir What directory should each chapter file be written in? By default, a subdirectory of the folder containing \code{Report.tex}, called \code{tex}, is used.
#' @param use.chapter.title Should the chapter title be used to name the chapter files? If \code{TRUE}, the default, the title is used (with characters outside \code{[a-zA-Z0-9]} replaced by spaces), prefixed by the chapter number; otherwise, just the chapter number is used.
#' @param out.tex The new root file. By default, same as \code{Report.tex}.
#' @export

split_report <- function(Report.tex,
                         include = TRUE,
                         subdir = "tex",
                         use.chapter.title = TRUE,
                         out.tex = Report.tex) {
  Report <- readr::read_lines(Report.tex)
  chapter_lines <-
    grep("\\chapter", strip_comments(Report), fixed = TRUE)
  
  if (use.chapter.title) {
    chapter_contents <- 
      extract_mandatory_LaTeX_argument(Report[chapter_lines], "chapter") %>%
      .[["extract"]] %>%
      trimws
    
    if (!is.null(subdir) && subdir != ".") {
      hutils::provide.dir(dirname(Report.tex), subdir)
    }
  }
  
  for (chapter in seq_along(chapter_lines)) {
    if (chapter > 1L) {
      # chapter == 1 corresponds to lines before the first
      # \\chapter
      readr::write_lines(c(paste0("%!TEX root = ../",
                                  out.tex),
                           Report[seq(chapter_lines[chapter - 1L],
                                    chapter_lines[chapter] - 1L)]),
                         file.path(dirname(Report.tex),
                                   subdir,
                                   paste0(chapter - 1L,
                                          if (use.chapter.title) {
                                            paste0("-",
                                                   gsub("[^0-9A-Za-z]+", "-", 
                                                        chapter_contents[chapter - 1L], 
                                                        perl = TRUE))
                                          },
                                          ".tex")))
      
    }
  }
  
  end_document <- grep("\\end{document}", Report, fixed = TRUE)
  # do:
  chapter <- chapter + 1L
  readr::write_lines(c(paste0("%!TEX root = ../",
                              out.tex),
                       Report[seq(chapter_lines[chapter - 1L],
                                  end_document - 1L)]),
                     file.path(dirname(Report.tex),
                               subdir,
                               paste0(chapter - 1L,
                                      if (use.chapter.title) {
                                        paste0("-",
                                               gsub("[^0-9A-Za-z]+", "-", 
                                                    chapter_contents[chapter - 1L], 
                                                    perl = TRUE))
                                      },
                                      ".tex")))
  
  
  
  out <- Report
  preamble <- TRUE
  chapter <- 1
  for (i in seq_along(Report)) {
    if (i >= chapter_lines[1]) {
      if (i %notin% chapter_lines) {
        if (!grepl("\\end{document}", Report[i], fixed = TRUE)) {
          out[i] <- ""
        }
      } else {
        chapter <- chapter + 1
        out[i] <- paste0("\\include{", 
                         file.path(subdir,
                                   paste0(chapter - 1L,
                                          if (use.chapter.title) {
                                            paste0("-",
                                                   gsub("[^0-9A-Za-z]+", "-", 
                                                        chapter_contents[chapter - 1L], 
                                                        perl = TRUE))
                                          })),
                         "}")
      }
    }
    
    if (grepl("\\begin{document}", Report[i], fixed = TRUE)) {
      preamble <- FALSE
    }
  }
  
  readr::write_lines(out, out.tex)
}


