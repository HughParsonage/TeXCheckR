#' Extract optional argument
#' @param tex_lines A character vector reading from a LaTeX document.
#' @param command_name Name of command (without backslash)
#' @param n Which optional argument to extract.
#' @param by.line Should the output be one row per command (\code{FALSE}, the default), 
#' with extracts concatenated via \code{paste0(..., collapse = "")}
#' or one row per line per command?
#' @export

extract_optional_LaTeX_argument <- function(tex_lines,
                                            command_name,
                                            n = 1L, 
                                            by.line = FALSE) {
  tex_lines <- strip_comments(tex_lines, retain.percent.symbol = FALSE)
  
  char <- char_no <- line_no <- NULL
  parsed_doc <- parse_tex(tex_lines)
  
  nchar_command <- nchar(command_name)
  command_split <- strsplit(command_name, split = "", fixed = TRUE)[[1]]
  sk <- seq_len(nchar_command)
  
  
  # Find the location of each command
  nchar_command <- nchar(command_name)
  command_split <- strsplit(command_name, split = "", fixed = TRUE)[[1]]
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
  for (k in sk) {
    set(parsed_doc_no_ws, j = as.character(k), value = shift(chars, n = k, type = "lag"))
  }
  backslash <- NULL
  set(parsed_doc_no_ws, j = "backslash", value = shift(chars, n = k + 1L, type = "lag"))
  
  
  shift_char <- command <- command_no <- NULL
  # Unlike extract_mandatory, we only care about '['.
  candidates <- 
    parsed_doc_no_ws[char == "["] %>% 
    .[backslash == "\\"] %>%
    melt.data.table(measure.vars = as.character(sk),
                    value.name = "shift_char",
                    variable.factor = FALSE,
                    na.rm = TRUE) %>%
    .[, list(command = paste0(rev(shift_char), collapse = "")), keyby = "char_no"] %>%
    .[command == command_name] %>%
    .[, command_no := .I]
  
  molten_parsed_doc <-
    melt.data.table(parsed_doc,
                    measure.vars = grep("^OPT_GROUP", names(parsed_doc), value = TRUE))
  
  if (nrow(candidates) == 0) {
    out <- data.table()
  } else {
    
    .group_id_noms <- grep("^OPT_GROUP_ID[1-9]",
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
    
    char_no_min <- char_no_max <- command_no_t <- target <- NULL
    
    # Must be outer join
    candidate_char_ranges <- 
      candidates[molten_parsed_doc, on = "char_no"] %>%
      
      # Only include NAs after the first command
      .[cumsum(!is.na(command)) > 0] %>%
      
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
    
    if (nrow(candidate_char_ranges) == 0) {
      # Under this condition, the nth argument of a command
      # has been requested, but is not present 
      out <- data.table()
    } else {
      
      column <- NULL
      column_by_char_no <- parsed_doc[, .SD, .SDcols = c("char_no", "column")]
      
      extract <- NULL
      out <-
        candidate_char_ranges[parsed_doc,
                              on = c("char_no_min<=char_no", "char_no_max>=char_no"),
                              nomatch=0L] %>%
        .[, .(extract = paste0(char, collapse = ""),
              line_no_min = min(line_no),
              line_no_max = max(line_no),
              char_no_min = min(char_no_min),
              char_no_max = max(char_no_max)),
          by = c("command_no", if (by.line) "line_no")]
      
      if (by.line) {
        out[, c("N", "I") := list(.N, seq_len(.N)), by = "command_no"]
        out[I == 1L, extract := stri_sub(extract, 2L)]
        out[I == N, extract := stri_sub(extract, to = -2L)]
      } else {
        out[, extract := stri_sub(extract, 2L, -2L)]
      }
    }
  }
  out[]
  
  
}






