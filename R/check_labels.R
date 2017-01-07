#' Check labels
#' @param filename The LaTeX source file to check.
#' @details Checks each label has a prefix and the prefix is one of the following:
#' \code{fig:},
#' \code{tbl:},
#' \code{box:},
#' \code{chap:},
#' \code{sec:},
#' \code{subsec:},
#' \code{subsubsec:},
#' \code{subsubsec:},
#' \code{para:}
#' \code{paragraph:}.
#' Checks also that chapter labels are marked with \code{chap:}.
#' @return \code{NULL}, invisibly if labels check out. An error otherwise.
#' @export

check_labels <- function(filename){
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)

  lines <- gsub("[%].*$", "", lines,perl = TRUE)

  lines_with_labels <- grep("\\label", lines, fixed = TRUE)
  label_contents <-
    lines[lines_with_labels] %>%
    strsplit(split = "\\", fixed = TRUE) %>%
    vapply(function(commands){
      grep("^label", commands, perl = TRUE, value = TRUE) %>%
        gsub(pattern = "^label[{]([^\\}]+)[}].*$", replacement = "\\1", x = ., perl = TRUE)
    }, FUN.VALUE = character(1))

  wrong_lines <-
    lines_with_labels %>%
    .[!grepl("^((fig)|(tbl)|(box)|(chap)|((sub){0,2}sec)|(para(graph)?)|(rec))[:]",
             label_contents,
             perl = TRUE)]

  if (length(wrong_lines) > 0){
    first_wrong_line <- wrong_lines[[1]]
    cat(bgRed(symbol$cross), " ",
        first_wrong_line, ": ", lines[first_wrong_line],
        sep = "")
    stop("Each \\label should contain a prefix.")
  }

  # Match label and command?
  # Probably not necessary, except for chapter etc
  chapter_label_lines <-
    lines[lines_with_labels[grepl("^chap[:]", label_contents)]]

  chapter_line_nos <-
    sort(union(grep("\\addchap", lines, fixed = TRUE),
               grep("\\chapter", lines, fixed = TRUE)))

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
    stop("Chapters must be labelled and have prefix 'chap:'.")
  }
  invisible(NULL)
}
