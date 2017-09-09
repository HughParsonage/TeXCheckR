#' Extract LaTeX command argument
#' @param tex_lines LaTeX text.
#' @param command_name Name of command without backslash \code{\\textbf} corresponds to \code{command_name = "textbf"}.
#' @param n Extract the nth argument.
#' @param optional Extract the optional argument, rather than the mandatory arguments.
#' @export

extract_LaTeX_argument <- function(tex_lines, command_name, n = 1L, optional = FALSE) {
  if (optional) {
    .NotYetUsed("optional")
  }
  nth_arg_pos <- nth_arg_positions(tex_lines = tex_lines, command_name = command_name, n = n)
  
  lapply(seq_along(nth_arg_pos), function(e) {
    out <- dt_e <- nth_arg_pos[[e]] 
    NN <- nrow(out[complete.cases(out)])
    if (NN > 0) {
      ostart <- .subset2(out, "starts")
      ostop <- .subset2(out, "stops")
      for (i in seq_len(NN)) {
        set(out, i = i, j = "extract", value = substr(tex_lines[[e]], ostart[i], ostop[i]))
      }
      out[, extract := gsub("^\\{|\\}$", "", extract, perl = TRUE)]
    } else {
      out[, extract := NA_character_]
    }
    out[, line_no := e]
  }) %>%
    rbindlist
}
