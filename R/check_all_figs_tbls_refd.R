#' Return unreferenced figures or tables in document
#' @description Useful for checking whether all the figures and tables in a document have been 
#' referenced in the main text. You may exclude figures and tables from the check by using
#' the directive 
#' \code{\% may_be_left_unreferenced: }
#' in the preamble before the label that is to be excluded.
#' @param filename A LaTeX file.
#' @param .report_error A function to provide context to any errors.
#' @param check.labels if \code{TRUE}, the default, run \code{\link{check_labels}} on \code{filename} to ensure the figure and table
#' labels in \code{filename} are in the expected form or style. Set to \code{FALSE}
#' for possibly faster runs but the risk of spurious results.
#' @return The labels of any figure or table left unreferenced in \code{filename} (including inputs).
#' @export

figs_tbls_unrefd <- function(filename, .report_error, check.labels = TRUE){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  if (check.labels) {
    check_labels(filename)
  }
  
  lines <- read_lines(filename)
  
  may_be_left_unreferenced <- NULL
  if (any(grepl("^[%] may_be_left_unreferenced[:]", lines, perl = TRUE))){
    may_be_left_unreferenced <-
      grep("^[%] may_be_left_unreferenced[:]", lines, perl = TRUE, value = TRUE) %>%
      gsub("% may_be_left_unreferenced: ", "", ., fixed = TRUE) %>%
      stri_trim_both %>%
      strsplit(split = " ", fixed = TRUE) %>%
      unlist
  }
  
  lines <- strip_comments(lines)
  
  # Order is important (e.g. Vrefrange)
  input_pattern <- "^\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{](.*(?:\\.tex)?)[}]$"
  
  input_line_nos <- grep(input_pattern,
                         lines, 
                         perl = TRUE)
  
  file_path <- dirname(filename)
  while (length(input_line_nos)) {
    lines <- 
      lapply(seq_along(lines), 
             function(ii) {
               if (ii %in% input_line_nos) {
                 input_line <- lines[ii]
                 input_file <-
                   gsub(input_pattern,
                        "\\1.tex",
                        input_line,
                        perl = TRUE)
                 read_lines(file.path(file_path, input_file))
               } else {
                 lines[ii]
               }
             }) %>%
      unlist(use.names = FALSE)
    
    input_line_nos <- grep(input_pattern,
                           lines, 
                           perl = TRUE)
  }
  
  lines_with_labels <- grep("\\\\caption.*\\\\label", lines, perl = TRUE)
  
  figs_tbls_not_refd <- character(0)
  
  if (not_length0(lines_with_labels)){
    all_label_contents <-
      lines[lines_with_labels] %>%
      strsplit(split = "\\", fixed = TRUE) %>%
      vapply(function(commands){
        grep("^label", commands, perl = TRUE, value = TRUE) %>%
          gsub(pattern = "^label[{]([^\\}]+)[}].*$", replacement = "\\1", x = ., perl = TRUE)
      }, FUN.VALUE = character(1))
    
    label_contents <- setdiff(all_label_contents, may_be_left_unreferenced)
    
    if (not_length0(label_contents)) {
      ref_contents <- 
        lines %>%
        grep("ref{", ., fixed = TRUE, value = TRUE) %>%
        strsplit(split = " ", fixed = TRUE) %>%
        unlist %>%
        grep("ref{", ., fixed = TRUE, value = TRUE) %>%
        gsub("^.*ref[{]", "", ., perl = TRUE) %>%
        gsub("[}].*$", "", ., perl = TRUE) %>%
        # for Vref{fig:a,fig:b}
        strsplit(split = ",", fixed = TRUE) %>%
        unlist
      
      
      refrange_contents <- 
        lines %>%
        grep("refrange{", ., fixed = TRUE, value = TRUE) %>%
        strsplit(split = "(?<![!])\\\\(?=([VCcv]refrange))", perl = TRUE) %>%
        unlist %>%
        grep("^[VCcv]refrange", . , value = TRUE, perl = TRUE) %>%
        sub("^[VCcv]refrange\\{(.*?)\\}\\{(.*?)\\}.*$", "\\1 \\2", x = ., perl = TRUE) %>%
        strsplit(split = " ", fixed = TRUE)
      
      # Now need to all the ranges in case the Vrefrange
      # includes figures/tables between the endpoints
      
      # LaTeX guarantes that the figure order is the same 
      # *for the same environment* but not for figure*
      refrange_extent <- function(el) {
        # In case the VCrefrange doesn't include a captioned counter
        if (all(el %in% all_label_contents)) {
          lower <- which(all_label_contents == el[1])
          upper <- which(all_label_contents == el[2])
          all_label_contents[seq.int(lower, upper)]
        }
      }
      
      refrange_extents <- 
        refrange_contents %>%
        lapply(refrange_extent) %>%
        unlist
      
      ref_contents <- c(refrange_extents, ref_contents)
      
      if (is.null(ref_contents) || any(label_contents %notchin% ref_contents)) {
        fig_tbl_labels <-
          paste0("ref{", grep("^((fig)|tbl)[:]",
                              label_contents,
                              perl = TRUE,
                              value = TRUE))
        
        for (lab in fig_tbl_labels) {
          if (!any(grepl(lab, lines, fixed = TRUE))){
            lab <- gsub("ref{", "", lab, fixed = TRUE)
            figs_tbls_not_refd <- c(figs_tbls_not_refd, lab)
          }
        }
      }
    }
    
  }
  
  
  if (any(grepl("\\\\(?:(?:Chaps?ref)|(?:topref))", lines, perl = TRUE))){
    chapter_line_nos <-
      sort(union(grep("\\addchap", lines, fixed = TRUE),
                 grep("\\chapter", lines, fixed = TRUE)))
    
    labels_following_chapters <-
      sub("^.*\\\\label[{](.*[:][^\\}]*)[}].*$",
          "\\1",
          lines[chapter_line_nos],
          perl = TRUE)
    
    Chapref_targets <-
      grep("\\\\(?:Chapref(?!range))", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:Chapref(?!range)))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:Chapref(?!range))", commands, perl = TRUE, value = TRUE) %>%
          gsub(pattern = "^(?:Chapref(?!range))[{]([^\\}]+)[}].*$",
               replacement = "\\1",
               x = .,
               perl = TRUE)
      }) %>%
      unlist
    
    topref_targets <-
      grep("\\\\(?:topref)", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:topref))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:topref)", commands, perl = TRUE, value = TRUE) %>%
          sub(pattern = "^(?:topref)[{]([^\\}]+)[}].*$",
              replacement = "\\1",
              x = .,
              perl = TRUE)
      }) %>%
      unlist
    
    Chapref_range_1st <-
      grep("\\\\(?:Chaprefrange)", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:Chaprefrange))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:Chaprefrange)", commands, perl = TRUE, value = TRUE) %>%
          sub(pattern = "^(?:Chaprefrange)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
              replacement = "\\1",
              x = .,
              perl = TRUE)
      }) %>%
      unlist
    
    
    Chapref_range_2nd <-
      grep("\\\\(?:Chaprefrange)", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:Chaprefrange))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:Chaprefrange)", commands, perl = TRUE, value = TRUE) %>%
          sub(pattern = "^(?:Chaprefrange)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
              replacement = "\\2",
              x = .,
              perl = TRUE)
      }) %>%
      unlist
    
    
    Chapref_and_1st <-
      grep("\\\\(?:Chaprefand)", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:Chaprefand))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:Chaprefand)", commands, perl = TRUE, value = TRUE) %>%
          sub(pattern = "^(?:Chaprefand)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
              replacement = "\\1",
              x = .,
              perl = TRUE)
      }) %>%
      unlist
    
    
    Chapref_and_2nd <-
      grep("\\\\(?:Chaprefand)", lines, perl = TRUE, value = TRUE) %>%
      strsplit(split = "\\\\(?=(?:Chaprefand))", perl = TRUE) %>%
      lapply(function(commands){
        grep("^(?:Chaprefand)", commands, perl = TRUE, value = TRUE) %>%
          sub(pattern = "^(?:Chaprefand)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
              replacement = "\\2",
              x = .,
              perl = TRUE)
      }) %>%
      unlist
    
    
    Chapref_targets_all <- unique(c(Chapref_range_1st,
                                    Chapref_range_2nd),
                                  c(Chapref_and_1st,
                                    Chapref_and_2nd),
                                  c(topref_targets,
                                    Chapref_targets))
    
    if (!all(Chapref_targets_all %in% labels_following_chapters)){
      Chaprefs_undefined <-
        Chapref_targets[Chapref_targets %notin% labels_following_chapters]
      
      toprefs_undefined <-
        topref_targets[topref_targets %notin% labels_following_chapters]
      
      Chapref_range_1st_undefined <-
        Chapref_range_1st[Chapref_range_1st %notin% labels_following_chapters]
      
      Chapref_range_2nd_undefined <-
        Chapref_range_2nd[Chapref_range_2nd %notin% labels_following_chapters]
      
      Chapref_and_1st_undefined <-
        Chapref_and_1st[Chapref_and_1st %notin% labels_following_chapters]
      
      Chapref_and_2nd_undefined <-
        Chapref_and_2nd[Chapref_and_2nd %notin% labels_following_chapters]      
      
      
      offending_xrefs <- character(0)
      if (not_length0(Chaprefs_undefined)){
        offending_xrefs <-
          c(offending_xrefs,
            paste0('\\Chapref{', Chaprefs_undefined, '}'))
      }
      
      if (not_length0(toprefs_undefined)){
        offending_xrefs <-
          c(offending_xrefs,
            paste0("\\topref{", toprefs_undefined, "}"))
      }
      
      if (not_length0(Chapref_range_1st)){
        offending_xrefs <-
          c(offending_xrefs,
            paste0("\\topref{", Chapref_range_1st_undefined, "}"))
      }
      
      if (not_length0(Chapref_range_2nd)){
        ante_note <- "There were also empty cross-reference targets for the *2nd* argument of Chaprefrange."
      }
      
      
      if (not_length0(Chapref_and_1st)){
        offending_xrefs <-
          c(offending_xrefs,
            paste0("\\topref{", Chapref_and_1st_undefined, "}"))
      }
      
      if (not_length0(Chapref_and_2nd)){
        ante_note <- "There were also empty cross-reference targets for the *2nd* argument of Chaprefand."
      }
      
      
      error_message <- "Mislabeled or empty cross-references target for Chapref or topref"
      context <-
        paste0("Mislabeled / empty cross-reference target for a \\Chapref / \\topref etc. ",
               "You have entered a \\Chapref to cross-reference a chapter (which is correct!). ",
               "However the label you have referenced does not exist (perhaps it was renamed or removed?). ",
               "I saw:\n\t", paste0(offending_xrefs, collapse = "\n\t"), "\n",
               "Yet the only valid labels are:\n\t", paste0(labels_following_chapters, collapse = "\n\t"))
      
      .report_error(error_message = "Mislabeled or empty cross-references target for Chapref or topref",
                    context = context)
      stop(error_message, context)
    }
  }
  
  if (not_length0(figs_tbls_not_refd)) {
    figs_tbls_not_refd
  }
}
