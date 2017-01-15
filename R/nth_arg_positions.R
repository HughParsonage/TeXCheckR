#' Replace nth arguments
#' @name argument_parsing
#' @param tex_lines A character vector of a LaTeX file read in from readLines.
#' @param command_name The command name, without the initial backslash if \code{fixed = TRUE},
#' or the pattern of the command.
#' @param fixed Is \code{command_name} a pattern or a fixed expression?
#' @param n The number of mandatory arguments.
#' @param replacement What to replace the \code{n}th argument with.
#' @param .dummy_replacement An intermediate replacement value.
#'  This value cannot be present in \code{tex_lines}.

#' @rdname argument_parsing
#' @export replace_nth_LaTeX_argument
replace_nth_LaTeX_argument <- function(tex_lines,
                                       command_name,
                                       fixed = TRUE,
                                       n = 1L,
                                       replacement = "correct",
                                       .dummy_replacement = "Qq"){
  # Idea:
  ## 1. Find those lines with the command name
  ## 2. Split the lines and locate the character positions of the nth argument.
  ## 3. Replace all of those positions with .dummy_replacement.
  ## 4. Concatanate (back together) the lines then replace any
  ##    repetition of .dummy_replacement with replacement.

  stopifnot(length(command_name) == 1L)

  # For convenience only
  if (fixed){
    command_name <- sprintf("\\%s", command_name)
  }

  line_nos_w_command <-
    grep(command_name, tex_lines, fixed = fixed)

  tex_lines_with_command_name <-
    tex_lines[line_nos_w_command]

  if (any(grepl(.dummy_replacement, tex_lines_with_command_name, fixed = TRUE))){
    stop(".dummy_replacement occurs in tex_lines. Change .dummy_replacement.")
  }

  tex_lines_with_command_name_split <-
    tex_lines_with_command_name %>%
    strsplit(split = "")

  positions_of_nth_arg <-
    nth_arg_positions(tex_lines = tex_lines_with_command_name,
                      command_name = command_name,
                      fixed = fixed,
                      n = n)

  for (el in seq_along(tex_lines_with_command_name_split)){
    intervals <- positions_of_nth_arg[[el]]
    intervals[, "width" := stops - starts]

    if (any(intervals[["width"]] == 0L)){
      stop("Zero width interval not replaced.")
    }

    starts <- positions_of_nth_arg[[el]][["starts"]]
    stops  <- positions_of_nth_arg[[el]][["stops"]]
    for (row in 1:nrow(intervals)){
      tex_lines_with_command_name_split[[el]][
        seq.int(starts[row], stops[row])
        ] <- .dummy_replacement
    }

  }

  corrected_tex_lines <-
    vapply(tex_lines_with_command_name_split,
           paste0, collapse = "",
           FUN.VALUE = character(1)) %>%
    gsub(sprintf("(%s)+", .dummy_replacement),
         replacement,
         x = .,
         perl = TRUE)

  tex_lines[line_nos_w_command] <- corrected_tex_lines
  tex_lines
}


#' @rdname argument_parsing
#' @export nth_arg_positions
nth_arg_positions <- function(tex_lines, command_name, fixed = TRUE, n = 1L){
  Command_locations <-
    stringi::stri_locate_all_regex(str = tex_lines,
                                   # If command = \a, must not also detect \ab
                                   pattern = if (fixed){
                                     sprintf("\\\\%s(?![A-Za-z])", command_name)
                                   } else {
                                     sprintf("%s(?![A-Za-z])", command_name)
                                   })

  Tex_line_split <- strsplit(tex_lines, split = "")

  lapply(seq_along(tex_lines), function(i){
    command_locations <- Command_locations[[i]][, 2]
    tex_line_split <- Tex_line_split[[i]]
    tex_group <- cumsum(tex_line_split == "{") - cumsum(tex_line_split == "}")
    tex_group_lag <- dplyr::lag(tex_group, n = 1L, default = tex_group[[1]])
    tex_group_at_command_locations <- tex_group[command_locations]

    starts <- stops <- integer(length(command_locations))
    for (j in seq_along(command_locations)){
      tg <- command_locations[[j]]
      starts[[j]] <-
        1L + # below tells us the position of the opening *brace*
        nth_min.int(which(and(and(tex_group == tex_group[tg] + 1,
                                  seq_along(tex_group) > tg),
                              tex_group == tex_group_lag + 1)),
                    n = n)

      stops[[j]] <-
        nth_min.int(which(and(tex_group == tex_group[tg],
                              and(seq_along(tex_group) > tg,
                                  tex_group == tex_group_lag - 1))),
                    n = n) - 1L
    }
    data.table::data.table(starts = starts, stops = stops)
  })
}
