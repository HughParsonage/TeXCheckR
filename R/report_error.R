#' Report errors to console and twitter
#' @name report_error
#' @param preamble Message to appear before other messages.
#' @param line_no The line number locating the source of the error.
#' @param context THe content of the file to provide context to the error.
#' @param error_message The error message to display beyond the console.
#' @param advice Advice to the user: how should the detected error be resolved in general?
#' @param report_name Name of project whose errors are being reported.
#' @param build_status What should the build status be reported as?
#' @param authors Text to alert the authors (such as a twitter handle).
#' @param globalEnv The environment in which tweet statuses should be assigned. 
#' The default, \code{NULL}, is an error. The environment must be set by the user
#' to comply with (reasonable) CRAN requirements to not interfere with the user's 
#' environment.
#' @importFrom twitteR updateStatus
#' @param extra_cat_ante Character vector extra messages (placed before \code{context}).
#' @param extra_cat_post Character vector extra messages (placed after \code{context}).

#' @rdname report_error
report2console <- function(line_no = NULL,
                           context = NULL,
                           error_message = NULL,
                           advice = NULL,
                           build_status = NULL,
                           authors = NULL,
                           report_name = NULL,
                           extra_cat_ante = NULL,
                           extra_cat_post = NULL, 
                           log_file = NULL){
  # Printing requirements:
  ## 1. Cross
  ## 2. Line no (if applicable)
  ## 3. Context
  ## 4. Suggeston.
  
  # crayon::red(NULL) -> Error in mypaste(...) need character strings
  Red <- function(x) if (!is.character(x)) x else red(x)
  bold_red <- function(x) if (!is.character(x)) x else bold(red(x))
  cat("\n", 
      bold(red(error_message)), "\n",
      bold_red(symbol$cross), " ", Red(line_no), ": ", unlist(extra_cat_ante), Red(context), unlist(extra_cat_post), "\n",
      bold_red(advice), "\n",
      sep = "")
  
  # To return the directory if applicable
  if (!is.null(log_file)) {
    on.exit({
      if (file.exists(log_file)){
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
}




