#' Extract mandatory argument II
#' @param tex_lines A character vector of lines
#' @param parsed_doc A parsed document (from \code{\link{parse_tex}}).
#' @param command_name The command name (no backslash or opening brace).
#' @param n Which integer to 
#' @export

extract_mandatory_LaTeX_argument <- function(tex_lines, parsed_doc = NULL, command_name, n = 1L) {
  
  tex_lines <- strip_comments(tex_lines)
  if (AND(substr(tex_lines[[1]], 0, 1) == "[",
          stri_sub(last(tex_lines), from = -1L) == "]")) {
    tex_lines[1] <- paste0(" ", stri_sub(tex_lines[1], from = 2L))
    tex_lines[length(tex_lines)] <- paste0(stri_sub(tex_lines[length(tex_lines)], to = -2L), " ")
  }
  
  if (is.null(parsed_doc)) {
    parsed_doc <- parse_tex(tex_lines)#[tex_group >= possible_optional]
  }
  
  extracts_from_optional <- data.table()
  
  if (any(grepl("^OPT", names(parsed_doc)))) {
    optional_chars <-
      parsed_doc %>%
      melt.data.table(measure.vars = grep("^OPT", names(.), value = TRUE)) %>%
      .[!is.na(value)] 
    
    optional_char_nos <- .subset2(optional_chars, "char_no")
    
    extracts_within_optional <- 
      optional_chars %>%
      .[,
        .(text = paste0(char, collapse = ""),
          init_char_no = first(char_no)),
        by = c("variable", "value")] %>%
      .[grepl(sprintf("\\\\%s(?:\\s*(?:\\[|\\{))", command_name), text, perl = TRUE)] %>%
      # Optional groups have [] either side.
      # [ab[c]] -> ab[c] 
      .[, text := stri_sub(text, from = 2L, to = -2L)]
    
    if (nrow(extracts_within_optional) > 0) {
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
    
    parsed_doc <- parsed_doc_outside_optional <- parsed_doc[char_no %notin% optional_char_nos]
  } else {
    parsed_doc_outside_optional <- parsed_doc
  }
  
  
  
  # Find the location of each command
  nchar_command <- nchar(command_name)
  command_split <- strsplit(command_name, split = "", fixed = TRUE)[[1]]
  sk <- seq_len(nchar_command)
  # Add columns looking back
  # Where we get a hit across all rows, that's the GROUP_ID to capture.
  # Must go forward because optional arguments may confuse if we look behind
  # braces
  chars <- .subset2(parsed_doc, "char")
  
  for (k in sk) {
    set(parsed_doc, j = as.character(k), value = shift(chars, n = k, type = "lag"))
  }
  set(parsed_doc, j = "backslash", value = shift(chars, n = k + 1L, type = "lag"))
  
  # For each character in the command,
  # check whether the k'th position back
  # matches the k'th character in the command
  
  # Idea is to melt the data table so that variable K should have value command_split[k]
  # Fairly quick and avoids standard evaluation ;-)
  
  
  
  candidates <- 
    melt.data.table(parsed_doc[char == "{"],
                    measure.vars = as.character(sk),
                    value.name = "value",
                    variable.factor = FALSE) %>%
    .[, list(command = paste0(rev(value), collapse = "")), keyby = "char_no"] %>%
    .[command == command_name] %>%
    .[, command_no := .I]
  
  # Must be outer join
  candidates[parsed_doc, 
             on = "char_no"] %>%
    .[, .SD, .SDcols = c("line_no", "char_no", "char", "command", "command_no",
                         grep("^GROUP_ID[1-9]", names(.), value = TRUE))] %>%
    melt.data.table(measure.vars = grep("^GROUP_ID[1-9]", names(.), value = TRUE)) %>%
    .[!is.na(value)] %>%
    # n = 2
    # value = next group at this level
    #
    .[!is.na(command), value := as.integer(value + n - 1L)] %>%
    # NA values correspond to zero groups, which we can exclude
    .[, 
      c("command", "command_no") := lapply(.SD, fill_blanks),
      by = c("value", "variable"),
      .SDcols =  c("command", "command_no")] %>%
    .[!is.na(command)] %>%
    .[, .(extract = paste0(char, collapse = ""),
          init_line_no = first(line_no),
          init_char_no = first(char_no)),
      by = c("variable", "value", "command_no", "line_no")] %>%
    # {abc} -> abc
    # .[, I := seq_len(.N), by = command_no] %>%
    # .[, N := .N, by = command_no] %>%
    # # {abc -> abc
    # # not sure why from needs the if (n == 1) condition, just by observation.
    # .[I == 1, extract := stri_sub(extract, from = if (n == 1L) 2L else 3L), by = command_no] %>%
    # # def} -> def
    # .[I == N, extract := stri_sub(extract, to = -2L), by = command_no] %>%
    rbind(extracts_from_optional, use.names = TRUE, fill = TRUE)
  
}
