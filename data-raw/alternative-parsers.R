
parse_special <- function(parsed_doc, command_name) {
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
}

sieved_find2 <- function(needle, haystack) { 
  sieved <- which(haystack == needle[1L]) 
  for (i in seq_len(length(needle) - 1L)) {
    sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
  }
  sieved
}

sieved_find3 <- function(needle, haystack) {
  sieved <- which(haystack == needle[1L])
  sieved[sieved < length(haystack) - length(needle)]
  for (i in seq_len(length(needle) - 1L)) {
    sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
  }
  
  
}

library(fastmatch)
boyer_find <- function(needle, haystack) {
  needle_length <- length(needle)
  tails <- which(haystack == needle[needle_length])
  for (k in tails) {
    for ()
  }
}

sieved_find4 <- function(needle, haystack) {
  sieved <- which(haystack == needle[1L]) 
  length_needle <- length(needle)
  for (i in seq_len(length_needle - 1L)) {
    sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
  }
  # Look ahead for brace
  # sieved[!grepl("[[:alpha:]]", haystack[sieved + i + 1L])] + length_needle
  sieved[haystack[sieved + i + 1L] %chin% c("{", " ")] + length_needle
}

parse_jono <- function(parsed_doc, command_name) {
  needle <- c("\\", strsplit(command_name, split = "", fixed = TRUE)[[1L]])
  # parsed_doc_no_ws <- parsed_doc#[grep("^\\S$", char, perl = TRUE)]
  haystack <- .subset2(parsed_doc, "char")
  sieved <- sieved_find4(needle, haystack)
  out <- list(char_no = sieved,
              command = rep_len(command_name, length(sieved)),
              command_no = seq_along(sieved))
  setattr(out, "class", c("data.table", "data.frame"))
  setattr(out, "sorted", "char_no")
  alloc.col(out, n = 20L, verbose = FALSE)
  out
}



