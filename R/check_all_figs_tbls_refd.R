

check_all_figs_tbls_refd <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }

  lines <- readLines(filename)

  lines <- gsub("[%].*$", "", lines, perl = TRUE)

  lines_with_labels <- grep("\\label", lines, fixed = TRUE)
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
      .report_error(error_message = paste0("Couldn't find a xref to ", lab, "."))
      stop("Couldn't find a xref to ", lab, ".")
    }
  }

}
