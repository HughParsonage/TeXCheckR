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
                           extra_cat_post = NULL){
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
  on.exit({
    if (dir.exists("travis") && dir.exists("travis/grattanReport")){
      if (file.exists("./travis/grattanReport/error-log.tsv")){
        prev_build_status <-
          fread("./travis/grattanReport/error-log.tsv") %>%
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
        fwrite("./travis/grattanReport/error-log.tsv",
               sep = "\t",
               append = append)
    }
  }, add = TRUE)
}

#' @rdname report_error
report2twitter <- function(preamble = NULL,
                           report_name,
                           build_status,
                           error_message,
                           line_no = NULL,
                           context = NULL,
                           authors,
                           extra_cat_ante = NULL,
                           extra_cat_post = NULL,
                           globalEnv = NULL){
  if (is.null(globalEnv)){
    stop("Set globalEnv = .GlobalEnv when using this function so tweets can be deleted if sent by accident.")
  }
  # Printing requirements:
  ## 1. Each author
  ## 2. "<Report title> build failed/still failed/fixed"
  ## 3. Error message
  ## 4. Line no + context

  longest_author <- authors[which.max(nchar(authors))]

  possible_tweet <- c(longest_author,
                      paste(build_status, report_name),
                      paste0("\u2718", " ", error_message),
                      paste0(line_no, ": ", context))

  .Twitter.statuses <- list(length(authors))
  if (sum(nchar(possible_tweet)) <= 140){
    for (j in seq_along(authors)){
      tweet <- paste0(c(authors[[j]],
                        paste(build_status, report_name),
                        paste0("\u2718", " ", error_message),
                        paste0(line_no, ": ", context)),
                      collapse = "\n")
      .Twitter.statuses[[j]] <- updateStatus(text = tweet)
    }
    assign(".last.Twitter.status", .Twitter.statuses, envir = globalEnv)
    cat('.last.Twitter.status\n')
  } else {
    cat((possible_tweet))
    cat("######\n")
    tweet_nos <- (cumsum(nchar(possible_tweet)) %/% 140) + 1
  }
}

#' @rdname report_error
report2gmail <- function(preamble = NULL,
                         report_name,
                         build_status,
                         error_message,
                         line_no = NULL,
                         context = NULL,
                         authors,
                         extra_cat_ante = NULL,
                         extra_cat_post = NULL){
  name <- NULL
  email_addresses <- 
    Grattan_staff[name %in% authors] %>%
    .[["email_address"]]
  
  if (file.exists("./travis/grattanReport/gmailr-log.tsv")){
    prev_build_status <-
      fread("./travis/grattanReport/gmailr-log.tsv") %>%
      last %>%
      .[["build_status"]]
    build_status <-
      switch(prev_build_status, 
             "OK" = "Broken",
             "Broken" = "Still failing", 
             "Still failing" = "Still failing")
    append <- TRUE
  } else {
    build_status <- "Broken"
    append <- FALSE
  }
  
  the_message <-
    gmailr::mime(
      To = "hugh.parsonage@gmail.com", #email_addresses, 
      From = "hugh.parsonage@gmail.com",
      Subject = paste0(report_name, ": ", build_status)
    ) %>%
    gmailr::html_body(body = paste0("<html>", 
                                    "<body>", 
                                      "<p>",
                                        "grattanReporter reports the following error:",
                                      "</p>",
                                      "<p>",
                                        "&emsp;", error_message,
                                      "</p>",
                                      "<p>",
                                        "&emsp;", line_no, ": ", context,
                                      "</p>",
                                    "<p>Please fix.</p>",
                                    "</body>",
                                    "</html>"))
  gmailr::send_message(the_message)
  
  data.table(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             build_status = build_status, 
             error_message = error_message) %>%
  fwrite("./travis/grattanReport/gmailr-log.tsv",
         sep = "\t",
         append = append)
}


