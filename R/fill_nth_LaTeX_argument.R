


fill_nth_LaTeX_argument <- function(parsed_doc, command_names, n = 1L, optional = FALSE) {
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
  wi <- which(wi & .subset2(parsed_doc, "char") %notchin% c("{", "}"))
  set(parsed_doc, i = wi, j = "char", value = " ")
  parsed_doc
}



