


fill_nth_LaTeX_argument <- function(parsed_doc, command_names, n = 1L, optional = FALSE,
                                    return.text = TRUE) {
  char <- NULL
  for (command_name in command_names) {
    parsed_doc <- 
      locate_mandatory_LaTeX_argument(parsed_doc = parsed_doc,
                                      command_name = command_name,
                                      n = n)
  }
  
  wi <- rep_len(FALSE, nrow(parsed_doc))
  for (j in command_names) {
    wi <- wi | .subset2(parsed_doc, j)
  }
  if (any(wi)) {
    wi <- which(wi & .subset2(parsed_doc, "char") %notchin% c("{", "}"))
    if (length(wi)) {
      set(parsed_doc, i = wi, j = "char", value = " ")
    }
  }
  if (return.text) {
    .subset2(parsed_doc[, .(text = paste0(char, collapse = "")), keyby = "line_no"], 
             "text")
  } else {
    parsed_doc
  }
}



