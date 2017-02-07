

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

  lines_with_labels <- grep("\\\\caption.*\\\\label", lines, perl = TRUE)

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
          .report_error(error_message = "Unreferenced figure or table",
                        advice = paste0("Couldn't find a xref to ", lab, "."))
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

  if (any(grepl("\\\\(?:(?:Chaps?ref)|(?:topref))", lines, perl = TRUE))){
    chapter_line_nos <-
      sort(union(grep("\\addchap", lines, fixed = TRUE),
                 grep("\\chapter", lines, fixed = TRUE)))

    labels_following_chapters <-
      gsub("^.*\\\\label[{](.*[:][^\\}]*)[}].*$",
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
          gsub(pattern = "^(?:topref)[{]([^\\}]+)[}].*$",
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
          gsub(pattern = "^(?:Chaprefrange)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
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
          gsub(pattern = "^(?:Chaprefrange)[{]([^\\}]+)[}][{]([^\\}]+)[}].*$",
               replacement = "\\2",
               x = .,
               perl = TRUE)
      }) %>%
      unlist

    Chapref_targets_all <- union(union(Chapref_range_1st,
                                       Chapref_range_2nd),
                                 union(topref_targets,
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

  assign("all_figs_tbls_refd", value = all_figs_tbls_refd, pos = parent.frame())
  assign("figs_tbls_not_refd", value = figs_tbls_not_refd, pos = parent.frame())


  invisible(NULL)
}

