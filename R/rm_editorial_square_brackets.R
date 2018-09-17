#' Remove editorial square brackets
#' @description Change text such as \code{phas[e] out} to \code{phase out},
#' without removing square brackets denoting optional arguments.
#' @param tex_lines Lines (as from \code{readLines}).
#' @export
#' 
#' @examples 
#' x <- "the BCA's call to `urgently phas[e] out all side deals'"
#' rm_editorial_square_brackets(x)
#' 

rm_editorial_square_brackets <- function(tex_lines){
  # Assume square brackets are ordinary if they are not
  # preceded by a } or ] or a word preceded by a backslash
  gsub(paste0("(",
              "(?:\\s+[A-Za-z]*)",
              "|",
              "(?:^[A-Za-z]*)",
              "|",
              "(?:`)",  # e.g. # `[E]conomics.
              "\\s*",
              ")",
              "\\[",
              "(",
              "(?:\\w|\\s|\\d)+",
              ")",
              "\\]"),
       "\\1\\2",
       tex_lines,
       perl = TRUE)
}

