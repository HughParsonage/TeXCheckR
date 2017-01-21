

check_log <- function(path = ".", final = FALSE){
  log_files <- dir(path = path, pattern = "\\.log$", full.names = TRUE)
  if (length(log_files) != 1){
    stop("Path does not contain a single log file.")
  }

  log_file <- readLines(log_files[[1]])

  if (any(grepl("undefined references", log_file, fixed = TRUE))){
    stop(grep("undefined references",
              log_file,
              fixed = TRUE,
              value = TRUE)[[1]])
  }

  if (any(grepl("LaTeX Warning: There were multiply-defined labels.", log_file, fixed = TRUE))){
    stop("LaTeX Warning: There were multiply-defined labels.")
  }
  
  if (any(grepl("LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.",
                log_file,
                fixed = TRUE))){
    stop("LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.")
  }

  invisible(NULL)
}
