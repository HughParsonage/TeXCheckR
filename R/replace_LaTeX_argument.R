#' Replace contents of LaTeX argument
#' @param tex_lines Lines of text (as from \code{readLines}).
#' @param command_name Name of the command (without backslash) whose first
#'   argument will be replaced.
#' @param replacement Replacement for the contents of each argument.
#' @param .dummy_replacer What to replace every \strong{letter} in the argument with during parsing.
#' Selecting a \code{.dummy_replacer} that is present in \code{tex_lines} is an error.
#' @return A character vector with the first argument of \code{command_name} replaced with \code{replacement}.
#' @export replace_LaTeX_argument
#'

replace_LaTeX_argument <- function(tex_lines, command_name, replacement, .dummy_replacer = "ZzZz"){
  if (any(grepl(.dummy_replacer, tex_lines))){
    stop(".dummy_replacer occurs in tex_lines. Choose a different string.")
  }

  tex_lines_with_command <- grep(paste0("\\", command_name, "{"), tex_lines, fixed = TRUE)
  out <- tex_lines[tex_lines_with_command]

  dt.out <-
    lapply(seq_along(out),
           function(LL){
             line <- out[[LL]]
             starts <- positions_of_all_strings(line, command_name)
             stops <-
               vapply(starts,
                      FUN = function(x){
                        braces_closes_at(line, x)
                      },
                      FUN.VALUE = integer(1))
             data.table::data.table(start = starts,
                                    stop = starts + stops)
           })
  out_split <- strsplit(out, split = "", fixed = TRUE)
  out <-
    vapply(seq_along(out_split),
           FUN = function(i){
             x <- out_split[[i]]
             dt <- dt.out[[i]]
             x[Seq_union(dt$start, dt$stop - 1)] <- .dummy_replacer
             gsub(paste0("(", .dummy_replacer, ")+"), replacement, paste0(x, collapse = ""), perl = TRUE)
           },
           FUN.VALUE = character(1))


  tex_lines[tex_lines_with_command] <- out
  tex_lines
}
