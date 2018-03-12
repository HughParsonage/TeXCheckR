#' Replace nth arguments
#' @name argument_parsing
#' @param tex_lines A character vector of a LaTeX file (as read in from \code{readLines} or \code{readr::read_lines}).
#' @param command_name The command name, 
#' or the pattern of the command, without the initial backslash.
#' @param n Which argument of the command.
#' @param replacement What to replace the \code{n}th argument with.
#' @param optional If \code{FALSE}, the default, the \code{n}th mandatory argument is extracted.
#' If \code{TRUE}, the \code{n}th \emph{optional} argument is extracted.
#' @param warn If the nth argument is not present, emit a warning? Set to \code{FALSE} for n-ary commands.
#' @param .dummy_replacement An intermediate replacement value.
#'  This value cannot be present in \code{tex_lines}.

#' @rdname argument_parsing
#' @details \code{nth_arg_positions} reports the starts and stops of the command for every line.
#' This includes the braces (in order to accommodate instances where the argument is empty).
#' 
#' If the line is empty or does not contain the command the values of \code{starts} and \code{stops} are \code{NA_integer_}.
#' @export replace_nth_LaTeX_argument
#' 
#' @examples nth_arg_positions("This is a \\textbf{strong} statement.", "textbf")
#' replace_nth_LaTeX_argument("This is a \\textbf{strong} statement.", "textbf")
#' 
replace_nth_LaTeX_argument <- function(tex_lines,
                                       command_name,
                                       n = 1L,
                                       replacement = "correct",
                                       optional = FALSE,
                                       warn = TRUE,
                                       .dummy_replacement = "Qq"){
  # Idea:
  ## 1. Find those lines with the command name
  ## 2. Split the lines and locate the character positions of the nth argument.
  ## 3. Replace all of those positions with .dummy_replacement.
  ## 4. Concatanate (back together) the lines then replace any
  ##    repetition of .dummy_replacement with replacement.

  stopifnot(length(command_name) == 1L)

  line_nos_w_command <-
    grep(sprintf("\\\\%s(?![A-Za-z])", command_name), tex_lines, perl = TRUE)

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
                      n = n)

  should_warn <- FALSE
  for (el in seq_along(tex_lines_with_command_name_split)){
    intervals <- positions_of_nth_arg[[el]]
    
    if (!any(complete.cases(intervals))){
      if (warn){
        should_warn <- TRUE
      }
      break
    }

    starts <- .subset2(intervals, "starts")
    stops  <- .subset2(intervals, "stops")

    for (row in 1:nrow(intervals)){
      if (!.subset2(intervals, "zero_width")[row]){
        tex_lines_with_command_name_split[[el]][
          seq.int(starts[row] + 1L, stops[row] - 1L)
          ] <- .dummy_replacement
      } else {
        # {} --> { }
        #         ^
        tex_lines_with_command_name_split[[el]] <-
          c(tex_lines_with_command_name_split[[el]][seq.int(1L, starts[row])], 
            .dummy_replacement,
            tex_lines_with_command_name_split[[el]][-seq.int(1L, starts[row])])
      }
    }
    
    if (should_warn){
      warning("Skipped replacing a line as nth-argument was absent there.")
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
#' @param star Assume the starred version of the command. That is, assume that the contents of 
#' the argument lies on a single line.
#' @param data.tables Should each element of the list be a \code{data.table}? Set to \code{FALSE} 
#' for performance.
#' @export nth_arg_positions
nth_arg_positions <- function(tex_lines, command_name, n = 1L, optional = FALSE, star = TRUE, data.tables = TRUE) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    warning("Using non-stringi method. Run install.packages('stringi') for tested use.")
    return(extract_LaTeX_argument(tex_lines = tex_lines, command_name = command_name, n = n, optional = optional))
  } else {
  Command_locations <-
    stringi::stri_locate_all_regex(str = tex_lines,
                                   # If command = \a, must not also detect \ab
                                   pattern = sprintf("\\\\%s(?![A-Za-z])", command_name))
  nchar_tex_lines <- nchar(tex_lines)
  Tex_line_split <- strsplit(tex_lines, split = "")
  
  delim1 <- if (optional) "[" else "{"
  delim2 <- if (optional) "]" else "}"
  

  if (star) {
    out <- 
      lapply(seq_along(tex_lines), function(i) {
        # Where does the command name end (i.e. just before 
        # opening brace.
        command_locations <- Command_locations[[i]][, 2]
        tex_line_split <- Tex_line_split[[i]]
        if (nchar_tex_lines[i] > 0) {
          tex_group <- cumsum(tex_line_split == delim1) - cumsum(tex_line_split == delim2)
          tex_group_lag <- shift(tex_group, n = 1L, type = "lag", fill = tex_group[[1]])
          tex_group_at_command_locations <- tex_group[command_locations]
          
          starts <- stops <- integer(length(command_locations))
          for (j in seq_along(command_locations)){
            tg <- command_locations[[j]]
            starts[[j]] <-
              # below tells us the position of the opening *brace*
              nth_min.int(which(and(and(tex_group == tex_group[tg] + 1,
                                        seq_along(tex_group) > tg),
                                    tex_group == tex_group_lag + 1)),
                          n = n)
            
            stops[[j]] <-
              nth_min.int(which(and(tex_group == tex_group[tg],
                                    and(seq_along(tex_group) > tg,
                                        tex_group == tex_group_lag - 1))),
                          n = n)
          }
          list(starts = starts, stops = stops, zero_width = stops == starts + 1L)
        } else {
          list(starts = NA_integer_, stops = NA_integer_, zero_width = NA)
        }
      })
    
    if (data.tables) {
      lapply(out, setDT)
    } else {
      out
    }
  } else {
    n_char <- NULL
    
    char_no_by_line_no <- 
      data.table(line_no = seq_along(tex_lines),
                 n_char = shift(nchar_tex_lines, fill = 0L)) %>%
      .[, .(line_no, char_no = cumsum(n_char))]
    
    start <- end <- NULL
    intra_line_char_no <- end_char_no <- start_char_no <-
      next_start <- command_no <- NULL
    
    command_locations_by_char_no <-
      rbindlist(lapply(Command_locations, as.data.table), idcol = "line_no") %>%
      .[, intra_line_char_no := end + 1L] %>%
      .[char_no_by_line_no, on = "line_no"] %>%
      # Which character in the entire document
      # does (this) command's argument start at 
      # (where does this command's name end)
      .[, end_char_no := intra_line_char_no + char_no] %>%
      .[, start_char_no := start + char_no] %>%
      .[complete.cases(.)] %>%
      .[, next_start := shift(start_char_no, type = "lead", fill = .Machine$integer.max)] %>%
      .[, .(line_no, end_char_no,  next_start)] %>%
      .[, command_no := .I]
    
    Tex_line_split_unlist <- unlist(Tex_line_split)
    tex_groups <- cumsum(Tex_line_split_unlist == delim1) - cumsum(Tex_line_split_unlist == delim2)
    
    line_no <- char <- char_no <- tex_group <-
      initial_group_no <- NULL
    
    group_by_line_no <- 
      data.table(line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
                 char = Tex_line_split_unlist) %>%
      .[, char_no := .I] %>%
      .[, tex_group := cumsum(char == delim1) - cumsum(char == delim2)] %>%
      command_locations_by_char_no[., on = c("line_no<=line_no",
                                             "end_char_no<char_no",
                                             "next_start>char_no"),
                                   nomatch=0L] %>%
      unique %>%
      .[, initial_group_no := first(tex_group), by = "command_no"]
    
    group_by_line_no %>%
      .[tex_group >= initial_group_no] %>%
      .[, .(extract = paste0(char, collapse = ""),
            line_no = first(line_no)),
        by = .(command_no)] %>%
      unique(by = "command_no")
  }
  }
}


