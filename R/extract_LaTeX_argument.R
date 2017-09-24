#' Extract LaTeX command argument
#' @param tex_lines LaTeX text.
#' @param command_name Name of command without backslash \code{\\textbf} corresponds to \code{command_name = "textbf"}.
#' @param n Extract the nth argument.
#' @param star Assume that the command's contents are all on one line.
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

extract_LaTeX_argument <- function(tex_lines,
                                   command_name,
                                   n = 1L, 
                                   optional = FALSE, 
                                   star = TRUE) {
  delim1 <- if (optional) "[" else "{"
  delim2 <- if (optional) "]" else "}"
  if (star) {
    nth_arg_pos <- nth_arg_positions(tex_lines = tex_lines,
                                     command_name = command_name,
                                     n = n,
                                     optional = optional,
                                     star = TRUE,
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
  } else {
    # If command = \a, must not also detect \ab
    command_pattern <- sprintf("\\\\%s(?![A-Za-z])", command_name)
    
    if (!any(grepl(command_pattern, tex_lines, perl = TRUE))) {
      data.table(command_no = 0L,
                 extract = NA_character_,
                 line_no = 0L) %>% 
        .[line_no > 0]
    } else {
      
      nchar_tex_lines <- nchar(tex_lines)
      Tex_line_split <- strsplit(tex_lines, split = "")
      
      Tex_line_split_unlist <- unlist(Tex_line_split)
      tex_group <- cumsum(Tex_line_split_unlist == delim1) - cumsum(Tex_line_split_unlist == delim2)
      
      parsed_document <-
        setDT(list(line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
                   char = Tex_line_split_unlist,
                   tex_group = tex_group,
                   char_no = seq_along(Tex_line_split_unlist)))
      
      Command_locations <-
        stringi::stri_locate_all_regex(str = tex_lines,
                                       pattern = command_pattern)
      
      line_no <- n_char <- char_no <- NULL
      
      char_no_by_line_no <- 
        data.table(line_no = seq_along(tex_lines),
                   n_char = shift(nchar_tex_lines, fill = 0L)) %>%
        .[, .(line_no, char_no = cumsum(n_char))]
      
      # Where does the brace begin?
      tex_group_lag <- shift(tex_group, fill = first(tex_group))
      starts <- function(j) {
        j <- j - 1
        # below tells us the position of the opening *brace* given the end of the command name
        # \textbf{ but also need to consider \textbf   {.
        nth_min.int(which(and(and(tex_group == tex_group[j] + 1,
                                  seq_along(tex_group) > j),
                              tex_group == tex_group_lag + 1)),
                    n = n)
      }
      
      # Where does the brace close given the end of the *command name*
      stops <- function(j) {
        j <- j - 1
        nth_min.int(which(and(tex_group == tex_group[j],
                              and(seq_along(tex_group) > j,
                                  tex_group == tex_group_lag - 1))),
                    n = n)
      }
      
      intra_line_char_no <- end_char_no <- start_char_no <- NULL
      
      command_locations_by_char_no <-
        rbindlist(lapply(Command_locations, as.data.table), idcol = "line_no") %>%
        .[, "intra_line_char_no" := end + 1L] %>%
        .[char_no_by_line_no, on = "line_no"] %>%
        # Which character in the entire document
        # does (this) command's argument start at 
        # (where does this command's name end)
        .[, "end_char_no" := intra_line_char_no + char_no] %>%
        .[, "start_char_no" := start + char_no] %>%
        .[complete.cases(.)] %>%
        parsed_document[.,
                        j = list(char_no, line_no, end_char_no, start_char_no, tex_group),
                        on = "char_no==end_char_no"] %>%
        .[, "next_start" := shift(start_char_no, type = "lead", fill = .Machine$integer.max)] %>%
        .[, .(line_no, end_char_no, next_start, tex_group)] %>%
        .[, "command_no" := .I] %>%
        .[, "starts_at" := starts(end_char_no), by = command_no] %>%
        .[, "stops_at" := stops(end_char_no), by = command_no] %>%
        # If the command has zero arguments (or if the argument is optional
        # but not used \\footcite[][3]{Daley} vs \\footcite{Daley})
        .[]
     
      group_by_line_no <-
        parsed_document %>%
        # Should it match on tex_group only?
        command_locations_by_char_no[.,
                                     j = .(commands_line_no = line_no,
                                           starts = starts_at,
                                           stops = stops_at,
                                           line_no = i.line_no,
                                           char_no = i.char_no,
                                           command_no,
                                           tex_group,
                                           char),
                                     on = c("starts_at<=char_no",
                                            "stops_at>=char_no"),
                                     nomatch=0L,
                                     by=.EACHI,
                                     allow.cartesian=TRUE]
      
      out <-
        group_by_line_no %>%
        .[,
          .(extract = paste0(char, collapse = "")),
          keyby = .(command_no, line_no)] %>%
        .[, I := seq_len(.N), by = command_no] %>%
        .[, N := .N, by = command_no] %>%
        # {abc -> abc
        .[I == 1, extract := stri_sub(extract, from = 2L), by = command_no] %>%
        # def} -> def
        .[I == N, extract := stri_sub(extract, to = -2L), by = command_no] %>%
        .[]
      
      out
    }
  }
}


