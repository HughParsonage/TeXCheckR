#' Report errors to console and twitter
#' @name report_error
#' @param preamble Message to appear before
#' @param line_no The line number locating the source of the error.
#' @param context THe content of the file to provide context to the error.
#' @param authors Text to alert the authors (such as a twitter handle).
#' @param error.symbol The mark to indicate an error.
#' @importFrom twitteR updateStatus


#' @rdname report_error
#' @param ... Extra messages.
report2console <- function(line_no = NULL,
                           context = NULL,
                           error_message = NULL,
                           build_status = NULL,
                           authors = NULL,
                           report_name = NULL,
                           ...){
  # Printing requirements:
  ## 1. Cross
  ## 2. Line no (if applicable)
  ## 3. Context
  ## 4. Suggeston.
  cat(bgRed(symbol$cross), " ", line_no, ": ", context, ..., sep = "")
}


#' @rdname report_error
#' @param ... Extra messages.
report2twitter <- function(preamble = NULL,
                           report_name,
                           build_status,
                           error_message,
                           line_no = NULL,
                           context = NULL,
                           authors,
                           ...){
  # Printing requirements:
  ## 1. Each author
  ## 2. "<Report title> build failed/still failed/fixed"
  ## 3. Error message
  ## 4. Line no + context

  longest_author <- authors[which.max(nchar(authors))]

  possible_tweet <- c(longest_author,
                      paste(report_name, build_status),
                      error_message,
                      paste0("\u2718", " ",
                             "Line ", line_no, ": ",
                             context))

  .Twitter.statuses <- list(length(authors))
  if (sum(nchar(possible_tweet)) <= 140){
    for (j in seq_along(authors)){
      tweet <- paste0(c(authors[[j]],
                        paste(report_name, build_status),
                        error_message,
                        paste0("\u2718", " ",
                               "Line ", line_no, ": ",
                               context)),
                      collapse = "\n")
      .Twitter.statuses[[j]] <- updateStatus(text = tweet)
    }
    .last.Twitter.status <<- .Twitter.statuses
    cat('.last.Twitter.status\n')
  } else {
    cat((possible_tweet))
    cat("######\n")
    tweet_nos <- (cumsum(nchar(possible_tweet)) %/% 140) + 1
  }
}


