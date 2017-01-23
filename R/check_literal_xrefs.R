


check_literal_xrefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  
  # excl_citations
  lines <- gsub(paste0("cite", 
                       "(",
                       # prenote-postnote
                       "\\[",
                       "\\]",
                       "\\[", 
                       "[^\\]+]", 
                       "\\]", 
                       ")", 
                       "\\{"),
                "cite[][ignored]{", 
                lines, 
                perl = TRUE)
    
  # Avoid negative lookbehind issue:
  lines <- gsub("\\\\((?:sub){1,2})section", "\\\\\\1S", lines, perl = TRUE)
  
  xref_pattern <- paste0("(?<!(\\\\))", # not a command or command sub
                         "(",
                         "(?:Chapter)", 
                         "|",
                         "(?:Subsubsection)",
                         "|",
                         "(?:Subsection)", 
                         "|",
                         "(?:Section)",
                         "|", 
                         "(?:Figure)",
                         "|",
                         "(?:Table)",
                         "|", 
                         "(?:Box)",
                         ")", 
                         "(?![:\\}])")
  if (any(grepl(xref_pattern, lines, perl = TRUE, ignore.case = TRUE))){
    line_no <- grep(xref_pattern, lines, perl = TRUE, ignore.case = TRUE)[[1]]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Hard-coded xref in document. All xrefs need to use \\Cref or \\Vref (or \\Chapref for cross-references to chapters).")
    stop("Hard-coded xref in document. All xrefs need to use \\Cref or \\Vref (or \\Chapref for cross-references to chapters).")
  }
  
}
