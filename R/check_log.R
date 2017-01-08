

check_log <- function(path = ".", final = FALSE){
  log_files <- dir(path = path, pattern = "\\.log$")
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
  invisible(NULL)
}
