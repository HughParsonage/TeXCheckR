#' Extract mandatory argument II
#' @param tex_lines A character vector of lines as read from a LaTeX document.
#' @param command_name The command name (no backslash or opening brace).
#' @param n Which integer to 
#' @param by.line If \code{FALSE}, the default, each row of the \code{data.table} returned 
#' has the entire contents of the argument in \code{extract} column. If \code{TRUE}, 
#' the contents is split as it is in the document; arguments over multiple lines in the 
#' document are split over multiple rows in the \code{data.table} returned.
#' @param parsed_doc A parsed document (from \code{\link{parse_tex}}).
#' \code{parse_tex}. Use this argument if the cost of running \code{parse_tex} is 
#' expensive (such as repeatedly over the same document).
#' @export

extract_mandatory_LaTeX_argument <- function(tex_lines,
                                             command_name,
                                             n = 1L,
                                             by.line = FALSE,
                                             parsed_doc = NULL) {

  
  # parse_tex adds these columns
  char <- char_no <- line_no <- NULL
  if (is.null(parsed_doc)) {
    tex_lines <- strip_comments(tex_lines, retain.percent.symbol = FALSE)
    parsed_doc <- parse_tex(tex_lines)
  }
  
  extracts_from_optional <- data.table()
  
  # melt.data.table
  value <- NULL
  
  if (any(startsWith(names(parsed_doc), "OPT"))) {
    optional_chars <-
      melt.data.table(parsed_doc,
                      measure.vars = grep("^OPT", names(parsed_doc), value = TRUE),
                      na.rm = TRUE)
    
    optional_char_nos <- .subset2(optional_chars, "char_no")
    
    text <- init_char_no <- NULL
    
    extracts_within_optional <- 
      optional_chars %>%
      .[,
        .(text = paste0(char, collapse = ""),
          init_char_no = char_no[1L]),
        by = c("variable", "value")] %>%
      .[grepl(sprintf("\\\\%s(?:\\s*(?:\\[|\\{))", command_name), text, perl = TRUE)] %>%
      # Optional groups have [] either side.
      # [ab[c]] -> ab[c] 
      .[, text := stri_sub(text, from = 2L, to = -2L)]
    
    if (nrow(extracts_within_optional)) {
      # # # # # # # # # # # 
      # Recursion:
      #
      extracts_from_optional <-
        rbindlist(list(extracts_from_optional,
                       extract_mandatory_LaTeX_argument(.subset2(extracts_within_optional, "text"),
                                                        command_name = command_name,
                                                        n = n) %>%
                         .[complete.cases(.)]),
                  use.names = TRUE,
                  fill = TRUE)
      #
      # # # # # # # # # # # 
    }
    
    
    parsed_doc <- parsed_doc[(!char_no %in% optional_char_nos)]
  }
  
  # Find the location of each command
  nchar_command <- nchar(command_name)
  command_split <- strsplit(command_name, split = "", fixed = TRUE)[[1L]]
  sk <- seq_len(nchar_command)
  
  # Since LaTeX gobbles whitespace following a command, we must 
  # gobble it to detect the location of the command. The information
  # we need is the char no, so it's okay to discard the rows, but not
  # to parse the document without whitespace. 
  parsed_doc_no_ws <- parsed_doc[grep("^\\S$", char, perl = TRUE)]
  
  # Add columns looking back
  # Where we get a hit across all rows, that's the GROUP_ID to capture.
  # Must go forward because optional arguments may confuse if we look behind
  # braces
  chars <- .subset2(parsed_doc_no_ws, "char")
  
  # For each character in the command,
  # check whether the k'th position back
  # matches the k'th character in the command
  
  # Idea is to melt the data table so that variable K should have value command_split[k]
  # Fairly quick and avoids standard evaluation ;-)
  
  # N.B. It is *not* faster to set on i (i.e. only the rows == "{")
  for (k in sk) {
    set(parsed_doc_no_ws, j = as.character(k), value = shift(chars, n = k, type = "lag"))
  }
  backslash <- NULL
  set(parsed_doc_no_ws, j = "backslash", value = shift(chars, n = k + 1L, type = "lag"))
  
  # melt.data.table value.name
  shift_char <- NULL
  command <- NULL
  # The location of the command opening
  # is where the char is { and the backslash
  # character is '\\'
  candidates <- 
    parsed_doc_no_ws[char == "{"] %>% 
    .[backslash == "\\"] %>%
    melt.data.table(measure.vars = as.character(sk),
                    value.name = "shift_char",
                    variable.factor = FALSE,
                    na.rm = TRUE) %>%
    .[, list(command = paste0(rev(shift_char), collapse = "")), keyby = "char_no"] %>%
    .[command == command_name] %>%
    .[, command_no := .I]
  
  command_no <- NULL
  
  if (nrow(candidates) == 0L) {
    out <- extracts_from_optional
  } else {
    
    .group_id_noms <- grep("^GROUP_ID[1-9]",
                           names(parsed_doc),
                           value = TRUE,
                           perl = TRUE) 
    
    GROUP_LEVEL <- id_at_group_level <- NULL
    
    molten_parsed_doc <-
      parsed_doc %>%
      .[, .SD, .SDcols = c("line_no", "char_no", "char",
                           .group_id_noms)] %>%
      melt.data.table(measure.vars = .group_id_noms,
                      variable.factor = TRUE,
                      variable.name = "GROUP_LEVEL",
                      # NA values correspond to zero groups, which we can exclude
                      na.rm = TRUE,
                      value.name = "id_at_group_level") %>%
      # Rely on factor.
      .[, GROUP_LEVEL := as.integer(GROUP_LEVEL)] %>%
      
      # Choose the highest group number
      setorderv(c("char_no", "GROUP_LEVEL", "id_at_group_level")) %>%
      unique(by = c("line_no", "char_no", "char"), fromLast = TRUE)
    
    command_no_t <- target <- NULL
    
    # Must be outer join
    candidate_char_ranges <- 
      candidates[molten_parsed_doc, on = "char_no"] %>%
      
      # Only include NAs after the first command
      .[cumsum(!is.na(command)) > 0L] %>%
      
      .[, .(char_no_min = min(char_no),
            char_no_max = max(char_no),
            command = return_first_nonNA(command),
            command_no = return_first_nonNA(command_no)), 
        by = c("GROUP_LEVEL", "id_at_group_level")] %>%
      .[, command_no_t := fill_blanks(command_no)] %>%
      # When command isn't NA, that's the correct GROUP LEVEL and 
      # the correct id_at_group_level for n = 1. For later arguments, 
      # we just need to increment the id at group level (because it must be 
      # the relevant argument).
      .[, target := and((id_at_group_level + 1L - n) == first(id_at_group_level[!is.na(command)]), 
                        GROUP_LEVEL == first(GROUP_LEVEL[!is.na(command)])), 
        by = "command_no_t"] %>%
      .[(target), .(command_no = command_no_t, char_no_min, char_no_max)]
    
    if (nrow(candidate_char_ranges) == 0L) {
      # Under this condition, the nth argument of a command
      # has been requested, but is not present 
      out <- extracts_from_optional
    } else {
      
      column <- NULL
      column_by_char_no <- parsed_doc[, .SD, .SDcols = c("char_no", "column")]
      
      
      extract <- 
        line_no_min <- line_no_max <-
        char_no_min <- char_no_max <- NULL
      
      out <-
        candidate_char_ranges[parsed_doc,
                              on = c("char_no_min<=char_no", "char_no_max>=char_no"),
                              nomatch=0L] %>%
        .[, .(extract = paste0(char, collapse = ""),
              line_no_min = min(line_no),
              line_no_max = max(line_no),
              char_no_min = min(char_no_min),
              char_no_max = max(char_no_max)),
          keyby = c("command_no", if (by.line) "line_no")]
      
      if (by.line) {
        out[, c("N", "I") := list(.N, seq_len(.N)), keyby = "command_no"]
        out[I == 1L, extract := stri_sub(extract, 2L)]
        out[I == N, extract := stri_sub(extract, to = -2L)]
      } else {
        out[, extract := stri_sub(extract, 2L, -2L)]
      }
      
      if (nrow(out)) {
        column <- i.column <- NULL
        column_by_char_no <- parsed_doc[, .SD, .SDcols = c("char_no", "column")]
        out[column_by_char_no, on = "char_no_min==char_no", nomatch=0L] %>%
          .[column_by_char_no, on = "char_no_max==char_no", nomatch=0L,
            j = list(command_no,
                     if (by.line) line_no,
                     extract,
                     line_no_min,
                     line_no_max,
                     column_min = column,
                     column_max = i.column,
                     char_no_min,
                     char_no_max)]
      } else {
        # If this point is reached, we have detected a command
        # without an n'th mandatory argument, e.g.
        #   \\abc[][\\abc[][e]]{f}
        #             >>>    ^^<
        data.table()
      }
      
      out <- rbind(out, extracts_from_optional, use.names = TRUE, fill = TRUE)
      
    }
  }
  
  out
}
