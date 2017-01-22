

check_preamble <- function(filename, .report_error, final = FALSE, release = FALSE){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  file_path <- dirname(filename)
  lines <- 
    readLines(filename, encoding = "UTF-8", warn = FALSE) %>%
    strip_comments %>%
    trimws
    
  
  if (!grepl("^\\\\documentclass.*\\{grattan\\}$", lines[[1]], perl = TRUE)){
    .report_error(line_no = 1, 
                  context = lines[[1]], 
                  error_message = "Line 1 was not \\documentclass[<options>]{grattan}")
    stop("Line 1 was not \\documentclass[<options>]{grattan}")
  }
  
  begin_document <- which(lines == "\\begin{document}") - 1L
  if (length(begin_document) != 1L){
    .report_error(error_message = "Missing \\begin{document}. (Must occur on a line alone.)")
    stop("Missing \\begin{document}.")
  }
  lines_before_begin_document <-
    lines[1:begin_document]
  
  if (!any(grepl("^\\\\addbibresource", lines_before_begin_document, perl = TRUE))){
    stop("\\addbibresource not present in document preamble. (Must not be merely present in an \\input .)")
  }
  
  if (any(grepl("\\input", lines_before_begin_document, fixed = TRUE))){
    # Ensure the only input in acknowledgements is tex/acknowledgements
    acknowledgements <- 
      paste0(lines_before_begin_document, collapse = " ") %>%
      gsub("^.*\\\\(acknowledgements)", "", ., perl = TRUE)
    
    if (any(grepl("\\input", acknowledgements, fixed = TRUE))){
      inputs <-
        gsub("^.*\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{]([^\\}]+(?:\\.tex)?)[}].*$",
             "\\1",
             acknowledgements,
             perl = TRUE)
      
      if (inputs[[1]] != "tex/acknowledgements"){
        stop("The only permitted \\input in the preamble after \\acknowledgements is \\input{tex/acknowledgements}")
      }
      
      lines_before_begin_document <- 
        c(lines_before_begin_document,
          readLines(file.path(file_path, "./tex/acknowledgements.tex"),
                    encoding = "UTF-8",
                    warn = FALSE))
    }
  }
  
  if (final){
    if (release){
      if (any(grepl("embargo", lines_before_begin_document, fixed = TRUE))){
        .report_error(error_message = "String 'embargo' found before \\begin{document} while attempting to release a report.")
        stop("String 'embargo' found before \\begin{document} while attempting to release a document.")
      }
      
      
      
      GrattanReportNumber <- grep("\\GrattanReportNumber", lines_before_begin_document, fixed = TRUE, value = TRUE)
      if (length(GrattanReportNumber) != 1L){
        if (length(GrattanReportNumber) == 0L){
          stop("\\GrattanReportNumber not found in preamble.")
        } else {
          stop("Multiple \\GrattanReportNumbers in document.")
        }
      }
      GrattanReportNumberArg <- gsub("^.*[{](.*)[}].*$", "\\1", GrattanReportNumber, perl = TRUE)
      
      current_year <- 
        if (!any(grepl("\\YEAR", lines_before_begin_document, fixed = TRUE))){
          year_provided <- FALSE
          format(Sys.Date(), "%Y")
        } else {
          year_provided <- TRUE
          year_line <- grep("\\YEAR", lines_before_begin_document, fixed = TRUE)
          if (length(year_line) != 1L){
            stop("Multiple \\YEAR provided.")
          }
          gsub("[^0-9]", "", lines_before_begin_document[year_line])
        }
      
      if (substr(GrattanReportNumberArg, 0, 4) != current_year){
        if (year_provided){
          stop("GrattanReportNumber using ", substr(GrattanReportNumberArg, 0, 4), 
               " for the year of publication, but today's date is ",
               Sys.Date(), 
               " and \\YEAR has not been specified.")
        } else {
          stop("GrattanReportNumber using ", substr(GrattanReportNumberArg, 0, 4), 
               " for the year of publication, but line ", year_line, " is ",
               lines_before_begin_document[year_line], ".")
        }
      }
      
      is.wholenumber <- function(x){
        x <- as.integer(x)
        and(!is.na(x),
            abs(x - round(x)) < .Machine$double.eps^0.5)
      }
      
      if (!is.wholenumber(gsub("^.{5}", "", GrattanReportNumberArg))){
        stop("GrattanReportNumber not in the form YYYY-z where z is an integer.")
      }
    }
    
    
    # Check authors
    if (!any(grepl("^\\{\\\\footnotesize", lines_before_begin_document, perl = TRUE))){
      stop("Lines from 'This report may be cited as:' to 'All material ... Unported License' must be \\footnotesize.")
    }
    
    if (!any(lines_before_begin_document == "All material published or otherwise created by Grattan Institute is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License\\par")){
      stop("License line not present and correct. Could not find (as a single line)\n>", 
           "All material published or otherwise created by Grattan Institute is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License\\par<")
    }
    licence_line <- which(lines_before_begin_document == "All material published or otherwise created by Grattan Institute is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License\\par")
    
    if (lines_before_begin_document[licence_line + 1] != "}"){
      stop("Line after licence line must be a closing brace.")
    }
    
    # Check ISBN (13-digit)
    # 978-1-925015-95-9
    
    isbn_line <- grep("^ISBN:", lines_before_begin_document, perl = TRUE, value = FALSE)
    
    if (length(isbn_line) != 1L){
      if (length(isbn_line) == 0L){
        .report_error(error_message = "Missing ISBN: line.")
        stop("Missing ISBN: line.")
      } else {
        .report_error(error_message = "Multiple ISBNs provided on lines ", which(lines_before_begin_document %in% isbn_line))
        stop("Multiple ISBNs provided.")
      }
    }
    
    if (isbn_line != licence_line - 2){
      stop("ISBN: line must be two lines before licence line.")
    }
    
    isbn <-
      lines_before_begin_document %>%
      .[isbn_line] %>%
      gsub("[^0-9]", "", ., perl = TRUE) %>%
      strsplit(split = "") %>%
      unlist %>%
      as.integer
    
    if (length(isbn) != 13){
      .report_error(context = isbn_line,
                    error_message = "ISBN provided did not have 13 digits.")
      stop("ISBN provided did not have 13 digits.")
    }
    
    w <- c(1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L)
    if (sum(isbn * w) %% 10 != 0){
      check_sum <- sum(isbn * w) %% 10
      .report_error(context = isbn_line, 
                    error_message = paste0("Invalid ISBN. Checksum was ", check_sum))
      stop(paste0("Invalid ISBN. Checksum was ", check_sum))
    }
    
    if (!OR(lines_before_begin_document[isbn_line - 3] == "This report may be cited as:",
            identical(lines_before_begin_document[isbn_line - c(4:3)],
                      c("This report may be cited as:", "\\newline")))){
      stop("'This report may be cited as:' not found on the 3rd or 4th before 'ISBN: '.")
    }
    
    project_authors <- get_authors(filename)
    project_authors_initials <- gsub("^([A-Z])[a-z]+ ", "\\1. ", project_authors, perl = TRUE)
    project_authors_reversed <- rev_forename_surname_bibtex(project_authors_initials) 
    project_authors_textcite <- paste0(paste0(project_authors_reversed[-length(project_authors_reversed)], collapse = ", "), 
                                       ", and ", 
                                       last(project_authors_reversed))
    
    recommended_citation <- 
      paste0(project_authors_textcite, " (", current_year, "). ", "\\emph{\\mytitle}. Grattan Institute.")
    
    
    if (lines_before_begin_document[isbn_line - 2] != recommended_citation){
      .report_error(error_message = paste0("Recommended citation should be two lines before ISBN: . ",
                                           "I expected the citation\n\t",
                                           recommended_citation, 
                                           "\nbut saw\n\t", lines_before_begin_document[isbn_line - 2]))
      stop("Recommended citation should be two lines before ISBN: . ",
           "I expected the citation\n\t",
           recommended_citation, 
           "\nbut saw\n\t", lines_before_begin_document[isbn_line - 2])
    }
    
    # Check todonotes hl
    todonotes_setinel <- function(filename){
      lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
      if (any(grepl("\\\\usepackage.*(?:(?:\\{todonotes\\})|(?:\\{soul\\}))", lines, perl = TRUE))){
        .report_error(error_message = paste0("final = TRUE but found string 'usepackage{todonotes}' or 'usepackage{soul}' in ", filename, ",",
                                             "most likely due to \\usepackage{todonotes}. ",
                                             "These strings are not permitted anywhere in the project ",
                                             "(even commented out or disabled) when preparing a final document."))
        
        stop(paste0("final = TRUE but found string usepackage{todonotes}' or 'usepackage{soul}' in ", filename, ",",
                    "most likely due to \\usepackage{todonotes}. ",
                    "These strings are not permitted anywhere in the project ",
                    "(even commented out or disabled) when preparing a final document."))
      }
      
      if (any(grepl("\\hl", lines, fixed = TRUE))){
        stop("Found command \\hl in project.")
      }
      invisible(NULL)
    }
    invisible(lapply(list.files(path = file_path, pattern = "\\.tex", recursive = TRUE, full.names = TRUE), todonotes_setinel))
  }
  
}

