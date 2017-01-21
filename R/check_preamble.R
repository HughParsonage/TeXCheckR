

check_preamble <- function(filename, .report_error, final = FALSE, release = FALSE){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  file_path <- dirname(filename)
  lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
  
  begin_document <- which(lines == "\\begin{document}") - 1L
  if (length(begin_document) != 1L){
    .report_error(error_message = "Missing \\begin{document}. (Must occur on a line alone.)")
    stop("Missing \\begin{document}.")
  }
  lines_before_begin_document <-
    lines[1:begin_document]
  
  
  
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
        stop("The only permitted \\input in \\acknowledgements is \\input{tex/acknowledgements}")
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
        if (any(grepl("\\YEAR", lines_before_begin_document, fixed = TRUE))){
          format(Sys.Date(), "%Y")
        } else {
          year_line <- grep("\\YEAR", lines_before_begin_document, fixed = TRUE, value = TRUE)
          if (length(year_line) != 1L){
            stop("Multiple \\YEAR provided.")
          }
          gsub("[^0-9]", "", year_line)
        }
      
      if (AND(substr(GrattanReportNumberArg, 0, 4) != current_year,
              !grepl("\\YEAR", lines_before_begin_document, fixed = TRUE))){
        stop("GrattanReportNumber using ", substr(GrattanReportNumberArg, 0, 4), 
             " for the year of publication, but today's date is ",
             Sys.Date(), 
             " and \\YEAR has not been specified.")
      }
      
      is.wholenumber <- function(x){
        x <- as.integer(x)
        and(!is.na(x),
            abs(x - round(x)) < .Machine$double.eps^0.5)
      }
      
      if (!is.wholenumber(gsub("^.{6}", "", GrattanReportNumberArg))){
        stop("GrattanReportNumber not in the form YYYY-z where z is an integer.")
      }
    }
    
    
    # Check ISBN (13-digit)
    # 978-1-925015-95-9
    
    isbn_line <- grep("^ISBN:", lines_before_begin_document, perl = TRUE, value = TRUE)
    
    if (length(isbn_line) != 1L){
      if (length(isbn_line) == 0L){
        .report_error(error_message = "Missing ISBN: line.")
        stop("Missing ISBN: line.")
      } else {
        .report_error(error_message = "Multiple ISBNs provided on lines ", which(lines_before_begin_document %in% isbn_line))
        stop("Multiple ISBNs provided.")
      }
    }
    
    isbn <-
      isbn_line %>%
      gsub("[^0-9]", "", ., perl = TRUE) %>%
      strsplit(split = "") %>%
      unlist %>%
      as.integer
    
    if (length(isbn) != 13){
      stop("ISBN provided did not have 13 digits.")
    }
    
    w <- c(1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 1L)
    if (sum(isbn * w) %% 10 != 0){
      check_sum <- sum(isbn * w) %% 10
      .report_error(error_message = "Invalid ISBN. Checksum was ", check_sum)
      stop("Invalid ISBN.")
    }
    
    # Check todonotes hl
    todonotes_setinel <- function(filename){
      lines <- readLines(filename, encoding = "UTF-8", warn = FALSE)
      if (any(grepl("usepackage(?:(?:\\{todonotes\\})|(?:\\{soul\\}))", lines, perl = TRUE))){
        .report_error(error_message = paste0("final = TRUE but found string 'usepackage{todonotes}' or 'usepackage{soul}' in ", filename, ",",
                                             "most likely due to \\usepackage{todonotes}. ",
                                             "These strings are not permitted anywhere in the project ",
                                             "(even commented out) when preparing a final document."))
        
        stop(paste0("final = TRUE but found string usepackage{todonotes}' or 'usepackage{soul}' in ", filename, ",",
                    "most likely due to \\usepackage{todonotes}. ",
                    "These strings are not permitted anywhere in the project ",
                    "(even commented out) when preparing a final document."))
      }
      invisible(NULL)
    }
    invisible(lapply(list.files(path = file_path, pattern = "\\.tex", recursive = TRUE, full.names = TRUE), todonotes_setinel))
  }
  
}

