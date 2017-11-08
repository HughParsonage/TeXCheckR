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
      .[["extract"]]
    
    if (subdir != ".") {
      hutils::provide.dir(dirname(Report.tex), subdir)
    }
  }
  
  for (chapter in seq_along(chapter_lines)) {
    if (chapter > 1L) {
      # chapter == 1 corresponds to lines before the first
      # \\chapter
      write_lines(Report[seq(chapter_lines[chapter - 1L],
                              chapter_lines[chapter] - 1L)],
                  file.path(dirname(Report.tex), subdir,
                            paste0("chapter-", chapter - 1L,
                                   if (use.chapter.title) {
                                     chapter_contents[chapter - 1L]
                                   },
                                   ".tex")))
      
    }
  }
  
  out <- Report
  preamble <- TRUE
  chapter <- 0
  for (i in seq_along(Report)) {
    if (i >= chapter_lines[1]) {
      if (i %notin% chapter_lines) {
        if (!grepl("\\end{document}", Report[i], fixed = TRUE)) {
          out[i] <- ""
        }
      } else {
        chapter <- chapter + 1
        out[i] <- paste0("\\include{tex/chapter-", chapter, "}")
      }
    }
    
    if (grepl("\\begin{document}", Report[i], fixed = TRUE)) {
      preamble <- FALSE
    }
  }
  
  readr::write_lines(out, out.tex)
}


