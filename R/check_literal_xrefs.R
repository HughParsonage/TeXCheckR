#' Check for hard-coded cross-references
#' @param filename The TeX file to check
#' @param .report_error How errors should be reported.
#' @return An error, or if none found, \code{NULL} invisibly.
#' @export
#' 


check_literal_xrefs <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  
  lines <- strip_comments(lines)
  
  # excl_citations
  lines <- gsub(r2("cites?", 
                   r4("(",
                      # prenote-postnote
                      r3("(?:",
                         r5("\\[",
                            "\\]",
                            "\\[", 
                            "[^\\]]*", 
                            "\\]"), 
                         ")?"), 
                      r3("\\{", 
                         "[^\\}]*", 
                         "\\}"), 
                      ")+")),
                "cite[][ignored]{<key>}", 
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
                         "(?![:\\}])",
                         "(( [0-9]))")
  if (any(grepl(xref_pattern, lines, perl = TRUE, ignore.case = TRUE))){
    line_no <- grep(xref_pattern, lines, perl = TRUE, ignore.case = TRUE)[[1]]
    context <- lines[line_no]
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Hard-coded xref in document. All xrefs need to use \\Cref or \\Vref (or \\Chapref for cross-references to chapters).")
    stop("Hard-coded xref in document. All xrefs need to use \\Cref or \\Vref (or \\Chapref for cross-references to chapters).")
  }
  
}
