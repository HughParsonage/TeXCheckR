#' Strip comments from LaTeX lines
#' @param lines Character vector of a LaTeX document.
#' @param retain.percent.symbol (logical, default: \code{TRUE}) Should the \verb{\%} symbol itself be stripped?
#' @return \code{lines} but with all text to the right of every unescaped \verb{\%} removed
#' @examples
#' 
#' some_lines <- c("Text. % A comment", "20\% of comments are % useful")
#' strip_comments(some_lines)
#' strip_comments(some_lines, retain.percent.symbol = FALSE)
#' @export strip_comments

strip_comments <- function(lines, retain.percent.symbol = TRUE) {
  if (retain.percent.symbol) {
    sub("(?<!(\\\\))[%].*$", "%", lines, perl = TRUE)
  } else {
    sub("(?<!(\\\\))[%].*$", "", lines, perl = TRUE)
  }
}
