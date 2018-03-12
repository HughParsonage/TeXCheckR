#' Report errors to console
#' @name report_error
#' @param file The file in which the error occurred.
#' @param line_no The line number locating the source of the error.
#' @param column The position on the line to identify the error (usually following the error).
#' @param context The content of the file, to provide context to the error.
#' @param error_message The error message to display beyond the console.
#' @param advice Advice to the user: how should the detected error be resolved in general?
#' @param build_status What should the build status be reported as?
#' @param log_file Optionally, path to a log file on which \code{error_message} will be written. 
#' @param rstudio If available, should the report be allowed to modify the RStudio session (for example, to pop to the location of the error)?
#' @param extra_cat_ante Character vector extra messages (placed before \code{context}).
#' @param extra_cat_post Character vector extra messages (placed after \code{context}).
#' @param silent (logical, default: \code{FALSE}) Suppress all output.
#' @param halt Should failures halt via \code{stop} or just display a message in the console?
#' @rdname report_error
#' @export
report2console <- function(file = NULL,
                           line_no = NULL,
                           column = NULL,
                           context = NULL,
                           error_message = NULL,
                           advice = NULL,
                           build_status = NULL,
                           extra_cat_ante = NULL,
                           extra_cat_post = NULL,
                           rstudio = FALSE,
                           log_file = NULL,
                           silent = FALSE,
                           halt = getOption("TeXCheckR.halt_on_error", FALSE)) {
  # Printing requirements:
  ## 1. Cross
  ## 2. Line no (if applicable)
  ## 3. Context
  ## 4. Suggeston.
  
  # crayon::red(NULL) -> Error in mypaste(...) need character strings
  Red <- function(x) if (!is.character(x)) x else red(x)
  bold_red <- function(x) if (!is.character(x)) x else bold(red(x))
  if (!silent && OR(getOption("TeXCheckR.capture.output", FALSE), !is_testing())) {
    cat("\n", 
        bold_red(error_message), "\n",
        bold_red(symbol$cross), " ", Red(line_no), ": ",
        unlist(extra_cat_ante), Red(context), unlist(extra_cat_post), "\n",
        bold_red(advice), "\n",
        sep = "")
    
    if (rstudio &&
        !is.null(file) &&
        interactive() &&
        rstudioapi::isAvailable() && !is_testing()) {
      rstudioapi::navigateToFile(file, line = line_no, column = if (is.null(column)) 1L else as.integer(column))
    }
  }
  
  # To return the directory if applicable
  if (!is.null(log_file)) {
    on.exit({
      if (file.exists(log_file)) {
        prev_build_status <-
          fread(log_file) %>%
          last %>%
          .[["build_status"]]
        append <- TRUE
      } else {
        prev_build_status <- "None"
        append <- FALSE
      }
      
      if (prev_build_status %in% c("Broken", "Still failing")){
        build_status <- "Still failing"
      } else {
        build_status <- "Broken"
      }
      
      data.table(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 build_status = build_status, 
                 error_message = if (is.null(error_message)) "(No error msg provided.)" else error_message) %>%
        fwrite(log_file,
               sep = "\t",
               append = append)
    }, add = TRUE)
  }
  
  if (halt) {
    stop(error_message)
  }
  
}




