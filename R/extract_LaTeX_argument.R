#' Extract LaTeX command argument
#' @param tex_lines LaTeX text.
#' @param command_name Name of command without backslash \code{\\textbf} corresponds to \code{command_name = "textbf"}.
#' @param n Extract the nth argument.
#' @param optional Extract the optional argument, rather than the mandatory arguments.
#' @return A \code{data.table}, each row corresponding to each line in \code{tex_lines}
#' and each instance of \code{command_name}. If multiple commands are on the same line,
#' subsequent instances of commands appear on subsequent rows.
#' \describe{
#' \item{\code{starts}}{Start position of the argument.}
#' \item{\code{stops}}{Stop position of the argument.}
#' \item{\code{extract}}{The argument extracted.}
#' \item{\code{line_no}}{The line number (with respect to \code{seq_along(tex_lines)}).}
#' }
#' @export

extract_LaTeX_argument <- function(tex_lines, command_name, n = 1L, optional = FALSE) {
  nth_arg_pos <- nth_arg_positions(tex_lines = tex_lines,
                                   command_name = command_name,
                                   n = n,
                                   optional = optional)
  
  lapply(seq_along(nth_arg_pos), function(e) {
    out <- dt_e <- nth_arg_pos[[e]] 
    NN <- nrow(out[complete.cases(out)])
    if (NN > 0) {
      ostart <- .subset2(out, "starts")
      ostop <- .subset2(out, "stops")
      for (i in seq_len(NN)) {
        set(out, i = i, j = "extract", value = substr(tex_lines[[e]], ostart[i], ostop[i]))
      }
      
      if (optional) {
        out[, extract := gsub("^\\[|\\]$", "", extract, perl = TRUE)]
      } else {
        out[, extract := gsub("^\\{|\\}$", "", extract, perl = TRUE)]
      }
    } else {
      out[, extract := NA_character_]
    }
    out[, line_no := e]
  }) %>%
    rbindlist
}
