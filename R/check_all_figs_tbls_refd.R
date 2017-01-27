

check_all_figs_tbls_refd <- function(filename, .report_error, compile = FALSE, pre_release = FALSE){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }

  lines <- read_lines(filename)
  
  lines <- strip_comments(lines)
  
  inputs_of_filename <- inputs_of(filename)
  # Only have to put all the lines into one object;
  # order is unimportant
  inputs <- inputs_of_filename
  filename_path <- dirname(filename)
  while (!is.null(inputs)){
    file_path <- dirname(inputs[[1]])
    input_lines <- 
      lapply(inputs, function(x) read_lines(file.path(filename_path, x))) %>%
      unlist %>%
      strip_comments
    
    lines <- c(lines, input_lines)
    inputs <- 
      file.path(filename_path, inputs) %>%
      # can't use vapply as might be NULL or char
      sapply(inputs_of, USE.NAMES = FALSE) %>%
      unlist
  }

  lines_with_labels <- grep("\\label", lines, fixed = TRUE)
  
  all_figs_tbls_refd <- TRUE
  figs_tbls_not_refd <- character(0)
  
  if (not_length0(lines_with_labels)){
    label_contents <-
      lines[lines_with_labels] %>%
      strsplit(split = "\\", fixed = TRUE) %>%
      vapply(function(commands){
        grep("^label", commands, perl = TRUE, value = TRUE) %>%
          gsub(pattern = "^label[{]([^\\}]+)[}].*$", replacement = "\\1", x = ., perl = TRUE)
      }, FUN.VALUE = character(1))
    
    fig_tbl_labels <-
      paste0("ref{", grep("^((fig)|tbl)[:]",
                          label_contents,
                          perl = TRUE,
                          value = TRUE))
    
    for (lab in fig_tbl_labels){
      if (!any(grepl(lab, lines, fixed = TRUE))){
        lab <- gsub("ref{", "", lab, fixed = TRUE)
        if (compile){
          .report_error(error_message = paste0("Couldn't find a xref to ", lab, "."))
          if (pre_release){
            stop("Couldn't find a xref to ", lab, ".")
          } else {
            warning("Couldn't find a xref to ", lab, ".")
          }
        } 
        all_figs_tbls_refd <- FALSE
        figs_tbls_not_refd <- c(figs_tbls_not_refd, lab)
      }
    }
    
  }
  assign("all_figs_tbls_refd", value = all_figs_tbls_refd, pos = parent.frame())
  assign("figs_tbls_not_refd", value = figs_tbls_not_refd, pos = parent.frame())
  invisible(NULL)
}

