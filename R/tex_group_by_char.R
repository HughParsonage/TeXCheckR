#' TeX group by character position
#' @description Opening a brace increases the 'group' in TeX. For example, in \code{a{bc}{d{e}}}
#' \code{a} is in group 0, \code{bc} in group 1 as is \code{d} and \code{e} is in group 2.
#' @param tex_lines Character vector of a document LaTeX.
#' @param optional If \code{FALSE} (the default), the groups are taken with respect to braces. 
#' If \code{TRUE}, square brackets are used (perhaps not associated with a command).
#' @return A list the same length as \code{lines}. Each element an integer vector indicating the 
#' TeX group at that position. 
#' 
#' For positions \strong{at} braces the \strong{upcoming} group is returned. 
#' So \code{a{b}} should return \code{0 1 1 0} (in its first element).
#' @examples 
#' tex_group_by_char("a{bc}{d{e}}")
#' @export


tex_group_by_char <- function(tex_lines, optional = FALSE) {
  nchar_tex_lines <- nchar(tex_lines)
  Tex_line_split <- strsplit(tex_lines, split = "")
  
  delim1 <- if (optional) "[" else "{"
  delim2 <- if (optional) "]" else "}"
  
  TeX_group_by_char <- vector(length = length(Tex_line_split), mode = "list")
  prev_group <- 0
  # Must be a for-loop not lapply because
  # the previous line's group is added to 
  # the current group.
  for (line_no in seq_along(Tex_line_split)) {
    tex_line <- Tex_line_split[[line_no]]
    tex_group <- cumsum(tex_line == delim1) - cumsum(tex_line == delim2) + prev_group
    TeX_group_by_char[[line_no]] <- tex_group
    prev_group <- tex_group[nchar_tex_lines[line_no]]
  }
  TeX_group_by_char
}


