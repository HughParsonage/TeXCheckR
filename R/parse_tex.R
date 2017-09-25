#' Parse LaTeX lines
#' @param tex_lines Character vector (as read from a \code{.tex} file).
#' @param delim1,delim2 Opening and closing characters for each group.
#' @return A \code{data.table} where each row identifies a unique character in \code{tex_lines}.
#' \describe{
#' \item{\code{line_no}}{Matches the index of \code{tex_lines}.}
#' \item{\code{char_no}}{The character within \code{line_no}.}
#' \item{\code{char}}{The character. A single character.}
#' \item{\code{tex_group}}{The TeX group by default. Any delimiters can be used.}
#' }
#' @export

parse_tex <- function(tex_lines, delim1 = "{", delim2 = "}") {
  Tex_line_split_unlist <- unlist(strsplit(tex_lines, split = "", fixed = TRUE))
  nchar_tex_lines <- nchar(tex_lines)
  
  setDT(list(char_no = seq_along(Tex_line_split_unlist),
             line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
             column = unlist(lapply(nchar_tex_lines, seq_len)),
             char = Tex_line_split_unlist,
             tex_group = cumsum(Tex_line_split_unlist == delim1) - cumsum(Tex_line_split_unlist == delim2)))
}

