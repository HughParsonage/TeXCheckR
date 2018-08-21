#' Check labels
#' @param filename The LaTeX source file to check.
#' @details Checks each label has a prefix and the prefix is one of the following:
#' \code{fig:},
#' \code{tbl:},
#' \code{box:},
#' \code{chap:},
#' \code{sec:},
#' \code{eq:},
#' \code{subsec:},
#' \code{subsubsec:},
#' \code{para:}
#' \code{paragraph:}.
#' Checks also that chapter labels are marked with \code{chap:}. 
#' (N.B. although each label must have a prefix, it must not necessarily the \emph{right} prefix; 
#' for example, a table caption may have prefix \code{tbl:}.)
#' @param .report_error The function to provide context to the error.
#' @param check.chaprefs (logical, default: \code{TRUE}) If \code{TRUE}, require all cross-references to use \code{\\Chapref}.
#' @return \code{NULL}, invisibly if labels check out. An error otherwise.
#' @export

check_labels <- function(filename, .report_error, check.chaprefs = TRUE) {
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  orig <- lines <- read_lines(filename)

  lines <- trimws(strip_comments(lines))
  

  # which.max faster in case there are (somehow) 2 \begin{documents}'s
  begin_at <- which.max(lines == "\\begin{document}")
  if (!length(begin_at)) {
    begin_at <- 0L
  }
  space_after_label <-"(\\\\label[^\\}]*)\\s[^\\}]*\\}"
  if (any(grepl(space_after_label, stri_trim_both(lines), perl = TRUE))) {
    line_no <- grep(space_after_label, stri_trim_both(lines), perl = TRUE)[[1]]
    nchars_b4 <- nchar(sub(paste0("^(.*)", space_after_label), "\\1\\2", lines[line_no], perl = TRUE))
    context <- paste0(stri_trim_both(lines[[line_no]]), "\n",
                      paste0(rep(" ", nchars_b4 + 5 + nchar(line_no)),
                             collapse = ""),
                      "^^")
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Space somewhere after \\label . Spaces are not permitted in \\label.")
    stop("Space somewhere after \\label. Spaces are not permitted in \\label.")
  }

  lines_with_labels <- grep("\\label{", lines, fixed = TRUE)
  
  lines_with_labels <- lines_with_labels[lines_with_labels > begin_at]
  
  label_contents <-
    lines[lines_with_labels] %>%
    strsplit(split = "\\", fixed = TRUE) %>%
    lapply(function(commands){
      grep("^label\\{", commands, perl = TRUE, value = TRUE) %>%
        gsub(pattern = "^label[{]([^\\}]+)[}].*$", replacement = "\\1", x = ., perl = TRUE)
    }) %>%
    unlist
  
  if (any(grepl("^app(endix)?[:]", label_contents, perl = TRUE))){
    line_no <- grep("\\\\label\\{app(endix)?[:]", lines, perl = TRUE)[[1]]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Appendix \\label using appendix:",
                  advice = "Appendix labels must not use \\label{appendix: or \\label{app: . Change to \\label{chap: , \\label{sec: etc, as required.")
    stop("Appendix labels must not use \\label{appendix: or \\label{app: . Change to \\label{chap: , \\label{sec: etc, as required.")
  }

  if (!all(grepl("^((fig)|(tbl)|(box)|(chap)|((sub){0,2}sec)|(para(graph)?)|(rec)|(fn)|(eq))[:]",
                 label_contents,
                 perl = TRUE))) {
    which_bad <- which(!grepl("^((fig)|(tbl)|(box)|(chap)|((sub){0,2}sec)|(para(graph)?)|(rec)|(fn)|(eq))[:]",
                              label_contents,
                              perl = TRUE))[[1L]]
    first_wrong_line <- lines_with_labels[[which_bad]]
    .report_error(file = filename,
                  line_no = first_wrong_line, 
                  context = lines[[first_wrong_line]], 
                  error_message = "\\label used without prefix.",
                  advice = "Use fig: tbl: box: chap: subsec: paragraph: rec: fn: in every label.")
    stop("Each \\label must contain a prefix.")
  }
  
  # Check all captions have a label
  caption_without_label <- 
    and(grepl("\\caption{", lines, fixed = TRUE), 
        !grepl("\\\\label\\{(?:fig)|(?:tbl)[:]", lines, perl = TRUE))
  
  caption_without_label[seq_len(begin_at)] <- FALSE
  
  if (any(caption_without_label)) {
    .report_error(file = filename,
                  line_no = which(caption_without_label)[[1]], 
                  context = lines[caption_without_label][[1]], 
                  error_message = "\\caption present without label.",
                  advice = "(All captions must have a \\label and the label must occur on the same line.)")
    stop("\\caption{} present without \\label{}")
  }

  if (check.chaprefs) {
    
    # Match label and command?
    # Probably not necessary, except for chapter etc
    chapter_label_lines <-
      lines[lines_with_labels[grepl("^chap[:]", label_contents)]]
    
    chapter_line_nos <-
      sort(union(grep("\\addchap{", lines, fixed = TRUE),
                 grep("\\chapter{", lines, fixed = TRUE)))
    
    if (length(begin.document <- grep("\\begin{document}", lines, fixed = TRUE))) {
      chapter_line_nos <- chapter_line_nos[chapter_line_nos > begin.document]
    }
    
    label_prefixes_following_chapters <-
      gsub("^.*\\\\label[{](.*)[:][^\\}]*[}].*$",
           "\\1",
           lines[chapter_line_nos],
           perl = TRUE)
    
    if (any(label_prefixes_following_chapters != "chap")){
      first_wrong_line_no <-
        chapter_line_nos %>%
        .[label_prefixes_following_chapters != "chap"] %>%
        .[1]
      
      
      cat(bgRed(symbol$cross), " ",
          first_wrong_line_no, ": ",
          lines[first_wrong_line_no],
          sep = "")
      .report_error(line_no = first_wrong_line_no, 
                    context = lines[[first_wrong_line_no]], 
                    error_message = "Unlabelled chapter or \\label without chap: prefix.", 
                    advice = "For every \\chapter{} ensure there is a \\label{chap:...} on the same line.")
      
      stop("Chapters must be labelled and have prefix 'chap:'.")
    }
    
    chapter_xref_lines <-
      grep("[VvCc]ref(range)?[{]chap[:]",
           lines,
           perl = TRUE)
    
    if (length(begin.document)) {
      chapter_xref_lines <- chapter_xref_lines[chapter_xref_lines > begin.document]
    }
    
    if (length(chapter_xref_lines) > 0){
      line_no <- chapter_xref_lines[[1]]
      .report_error(line_no = line_no,
                    context = lines[line_no],
                    error_message = "Cross-reference to chapter using Vref or Cref.",
                    advice = "Cross-references to chapters must use Chapref or topref.")
      stop("Cross-references to chapters must use Chapref or topref.")
    }
  }
  
  invisible(NULL)
}
