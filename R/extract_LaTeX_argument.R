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
                                   optional = optional, 
                                   data.tables = FALSE)
  
  lapply(seq_along(nth_arg_pos), function(e) {
    out <- nth_arg_pos[[e]]
    NN <- sum(complete.cases(out))
    
    # from data.table::setDT
    # Because we know all about out
    setattr(out, "class", c("data.table", "data.frame"))
    alloc.col(out)
   
    if (NN > 0) {
      ostart <- .subset2(out, "starts")
      ostop <- .subset2(out, "stops")
      for (i in seq_len(NN)) {
        set(out, i = i, j = "extract", value = substr(tex_lines[[e]], ostart[i], ostop[i]))
      }
      
      extract <- NULL
      
      set(out, j = "extract", value = stri_sub(.subset2(out, "extract"), from = 2L, to = -2L))
      
    } else {
      set(out, j = "extract", value = NA_character_)
    }
    set(out, j = "line_no", value = e)
  }) %>%
    rbindlist
}
