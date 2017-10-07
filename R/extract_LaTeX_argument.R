#' Extract LaTeX command argument
#' @description This is a simple wrapper around \code{\link{extract_mandatory_LaTeX_argument}} and \code{\link{extract_optional_LaTeX_argument}}.
#' @param tex_lines LaTeX text.
#' @param command_name Name of command without backslash \code{\\textbf} corresponds to \code{command_name = "textbf"}.
#' @param n Which argument to extract, if exists.
#' @param optional Extract the optional argument, rather than the mandatory arguments.
#' 
#' 
#' @export

extract_LaTeX_argument <- function(tex_lines,
                                   command_name,
                                   n = 1L, 
                                   optional = FALSE) {
  if (optional) {
    extract_optional_LaTeX_argument(tex_lines = tex_lines, command_name = command_name, n = n)
  } else {
    extract_mandatory_LaTeX_argument(tex_lines = tex_lines, command_name = command_name, n = n)
  }
}


