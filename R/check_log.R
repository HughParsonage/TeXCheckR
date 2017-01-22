

check_log <- function(path = ".", final = FALSE, check_for_rerun_only = FALSE){
  log_files <- dir(path = path, pattern = "\\.log$", full.names = TRUE)
  if (length(log_files) != 1){
    stop("Path does not contain a single log file.")
  }

  log_file <- readLines(log_files[[1]])
  
  file_list_start <- grep("*File List*", log_file, fixed = TRUE)
  stars <- grep("***********", log_file, fixed = TRUE)
  file_list_stop <- min(stars[stars > file_list_start])
  
  file_list <- log_file[file_list_start:file_list_stop]
  
  # >"  keyval.sty    2014/10/28 v1.15 key=value parser (DPC)"<
  file_list_pattern <- "^\\s*([a-z]+\\.(?:(?:cls)|(?:sty)))    ([12][0-9]{3}/[0-9]{2}/[0-9]{2}) (v?\\s?(?:er)?[0-9]+(?:\\.[0-9a-z]+)?)?.*$"
  
  file_list_grep <- grep(file_list_pattern, file_list, value = TRUE, perl = TRUE)
  
  file_tbl <- 
    data.table(package = gsub(file_list_pattern, "\\1", file_list_grep, perl = TRUE),
               date = as.Date(gsub(file_list_pattern, "\\2", file_list_grep, perl = TRUE), format = "%Y/%m/%d"),
               version = gsub(file_list_pattern, "\\3", file_list_grep, perl = TRUE))

  if (any(grepl("undefined references", log_file, fixed = TRUE))){
    stop(grep("undefined references",
              log_file,
              fixed = TRUE,
              value = TRUE)[[1]])
  }

  if (any(grepl("LaTeX Warning: There were multiply-defined labels.", log_file, fixed = TRUE))){
    stop("LaTeX Warning: There were multiply-defined labels.")
  }
  
  if (any(or(grepl("LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.",
                   log_file,
                   fixed = TRUE), 
             grepl("Please rerun LaTeX.",
                   log_file,
                   fixed = TRUE)))){
    if (check_for_rerun_only){
      return("Rerun LaTeX.")
    } else {
      stop("LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.")
    }
  }

  invisible(NULL)
}
