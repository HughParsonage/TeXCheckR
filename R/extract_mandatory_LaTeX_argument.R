#' Extract mandatory argument II
#' @param tex_lines A character vector of lines
#' @param command_name The command name (no backslash or opening brace).
#' @param n Which integer to 
#' @param parsed_doc A parsed document (from \code{\link{parse_tex}}).
#' @export

extract_mandatory_LaTeX_argument <- function(tex_lines, command_name, n = 1L, parsed_doc = NULL) {
  
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
                    value.name = "shift_char",
                    variable.factor = FALSE) %>%
    .[, list(command = paste0(rev(shift_char), collapse = "")), keyby = "char_no"] %>%
    .[command == command_name] %>%
    .[, command_no := .I] %>%
    .[]
  
  if (nrow(candidates) == 0) {
    out <- extracts_from_optional
  } else {
    
    molten_parsed_doc <-
      parsed_doc %>%
      .[, .SD, .SDcols = c("line_no", "char_no", "char",
                           grep("^GROUP_ID[1-9]", names(.), value = TRUE))] %>%
      melt.data.table(measure.vars = grep("^GROUP_ID[1-9]", names(.), value = TRUE),
                      variable.factor = FALSE,
                      variable.name = "GROUP_LEVEL",
                      value.name = "id_at_group_level") %>%
      .[, GROUP_LEVEL := as.integer(sub("^.*([0-9]+)$", "\\1", GROUP_LEVEL, perl = TRUE))] %>%
      # NA values correspond to zero groups, which we can exclude
      .[!is.na(id_at_group_level)] %>%
      # Choose the highest group number
      setorderv(c("char_no", "GROUP_LEVEL", "id_at_group_level")) %>%
      unique(by = c("line_no", "char_no", "char"), fromLast = TRUE)
    
    return_first_nonNA <- function(x) {
      if (anyNA(x) && any(!is.na(x))) {
        out <- first(x[!is.na(x)])
      } else {
        out <- first(x)
      }
      out
    }
    
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
      .[and((id_at_group_level + 1 - n) %in% id_at_group_level[!is.na(command)], 
            GROUP_LEVEL %in% GROUP_LEVEL[!is.na(command)])]
    
    if (nrow(candidate_char_ranges) == 0) {
      # Under this condition, the nth argument of a command
      # has been requested, but is not present 
      out <- extracts_from_optional
    } else {
      
      # Needs to be a range (so that nested arguments are included)
      out <- 
        candidate_char_ranges[, command_no := .I] %>% 
        .[parsed_doc, on = c("char_no_min<=char_no", "char_no_max>=char_no"), nomatch=0] %>%
        .[, .(extract = paste0(char, collapse = ""),
              line_no_min = min(line_no),
              line_no_max = max(line_no),
              column__min = min(column),
              column__max = max(column),
              char_no_min = min(char_no_min),
              char_no_max = max(char_no_max)),
          by = command_no] %>% 
        .[, extract := stri_sub(extract, 2L, -2L)] %>% 
        rbind(extracts_from_optional, use.names = TRUE, fill = TRUE)
      
    }
  }
  out
}
