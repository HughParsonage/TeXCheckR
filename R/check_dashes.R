#' Check dashes entered as hyphens
#'
#' @param filename A tex or Rnw file.
#' @param .report_error How errors should be reported.
#' @param dash.consistency Character vector permitted dash types. 
#' @param protases_ok (logical, default: \code{TRUE}) Should em-dashes be permitted when 
#' they form a protasis in a list? \code{\\item when there is an emdash---always.}
#' @param rstudio (logical, default: \code{TRUE}) Use the RStudio API?
#' @return File stops and \code{cat()}s on any line where a hyphen is surrounded by a space.
#' Excludes dashes in knitr chunks and LaTeX math mode \code{\(...\)} but not in TeX math mode \code{$...$}.
#' @export

check_dashes <- function(filename,
                         .report_error,
                         dash.consistency = c("en-dash", "em-dash"),
                         protases_ok = TRUE,
                         rstudio = TRUE) {
  if (missing(.report_error)) {
    if (rstudio) {
      .report_error <- function(...) report2console(...,
                                                    rstudio = TRUE,
                                                    file = filename)
    } else {
      .report_error <- function(...) report2console(...,
                                                    rstudio = FALSE)
    }
  }

  lines <- read_lines(filename)
  lines <- strip_comments(lines)

  lines[isR_line_in_knitr(lines)] <- "%"
  if ("\\begin{align*}" %chin% lines) {
    math_environ <- 
      which(cumsum(lines == "\\begin{align*}") - cumsum(lines == "\\end{align*}") == 1L)
    
    lines[math_environ] <- "% align environment"
  }
  
  display_equations <-
    cumsum(startsWith(trimws(lines), "\\[")) - 
    cumsum(endsWith(trimws(lines), "\\]")) > 0L
  
  lines[display_equations] <- "% equation environment"
  
  if (protases_ok) {
    which_protases <- grep("\\\\item (for|if|when|where)\\b", lines, perl = TRUE, ignore.case = TRUE)
    lines[which_protases] <- "ignored"
  }

  possible_hyphen <- grepl(" - ", lines, fixed = TRUE)

  if (any(possible_hyphen)){
    excluding_mathmode <-
      if_else(possible_hyphen,
              # Cheat: 
              #  - remove % signs (so they don't interfere later). This is ok as we've stripped comments
              #  - replace equations delimeters with % %. Struggled to match closing brackets otherwise (don't want to miss dashes between equations)
              #  - replace anything between %s with 
              gsub(paste0("%", "[^%]*", "%"),
                   "math-mode",
                   gsub("\\\\[()]", "%",
                        gsub("%",
                             "", 
                             lines),
                        
                        perl = TRUE),
                   perl = TRUE),
              lines)
    
    excluding_mathmode <-
      if_else(possible_hyphen,
              gsub("\\\\\\[.*$",
                   "",
                   excluding_mathmode,
                   perl = TRUE),
              excluding_mathmode)

    if (any(grepl(" - ", excluding_mathmode, fixed = TRUE))){
      line_no <- grep(" - ", excluding_mathmode, fixed = TRUE)[[1]]
      column <- stri_locate_first_fixed(lines[line_no], " - ")[1, 2]
      
      .report_error(line_no = line_no,
                    column = column,
                    context = lines[line_no],
                    error_message = "Dash likely masquerading as hyphen. Use -- for a dash.", 
                    advice = "\nIMPORTANT: make sure you are replacing a hyphen with two hyphens, not a unicode dash  \u2013\n",
                    "If you're not sure, reenter as two hyphens from the keyboard (rather than just appending a hyphen at the end). ",
                    "As always, visually check the result in the PDF.")
      stop("Single hyphen surrounded by spaces. This is likely a hyphen masquerading as dash. Use -- for a dash.\n",
           "\nIMPORTANT: make sure you are replacing a hyphen with two hyphens, not a unicode dash  \u2013\n",
           "If you're not sure, reenter as two hyphens from the keyboard (rather than just appending a hyphen at the end). ",
           "As always, visually check the result in the PDF.")
    }

  }

  # 35: Detect hyphens adjacent to unicode
  if (any(or(grepl("-\u2013", lines, fixed = TRUE),
             grepl("\u2013-", lines, fixed = TRUE)))){
    line_no <-
      which(or(grepl("-\u2013", lines, fixed = TRUE),
               grepl("\u2013-", lines, fixed = TRUE))) %>%
      .[1]
    
    if (grepl("\u2013-", lines[line_no], fixed = TRUE)) {
      column <- stri_locate_first_fixed(lines[line_no], "\u2013-")[1, 2]
    } else {
      column <- stri_locate_first_fixed(lines[line_no], "-\u2013")[1, 2]
    }
    if (is.null(column)) {
      column <- 1L
    }
    
    .report_error(line_no = line_no,
                  column = column,
                  context = lines[[line_no]],
                  error_message = "Hyphen adjacent to en-dash.")
    stop("Hyphen adjacent to en-dash. (Did you copy this line from Word?) ",
         "Make sure anything you intend as an en-dash is entered as ' -- '")
  }
  
  dash.consistency <-
    match.arg(dash.consistency, several.ok = !missing(dash.consistency))
  
  if ("em-dash" %notin% dash.consistency) {
    are_emdash_lines <-
      lines %>%
      grep("---", ., fixed = TRUE, value = TRUE) %>%
      gsub("\\{[^\\s\\}]+\\}", "\\{\\}", x = ., perl = TRUE) %>%
      grepl("---", ., fixed = TRUE)
    
    
    if (any(are_emdash_lines) || any(grepl("\u2014", lines, fixed = TRUE))){
      emdash_lines <-
        lines %>%
        gsub("\\{[^\\s\\}]+\\}", "\\{\\}", x = ., perl = TRUE) %>%
        grep("---", x = ., fixed = TRUE)
      
      emdash_lines <- union(emdash_lines,
                            grep("\u2014", lines, fixed = TRUE))
      line_no <- emdash_lines[[1]]
      .report_error(line_no = line_no,
                    context = lines[line_no],
                    error_message = "Em-dashes not permitted.")
      stop("Em-dashes not permitted.")
    }
  }
  
  if ("en-dash" %notin% dash.consistency) {
    are_emdash_lines <-
      lines %>%
      grep("(?<!(\\-))--(?!\\-)", ., perl = TRUE, value = TRUE) %>%
      gsub("\\{[^\\s\\}]+\\}", "\\{\\}", x = ., perl = TRUE) %>%
      grepl("(?<!(\\-))--(?!\\-)", ., perl = TRUE)
    
    
    if (any(are_emdash_lines) || any(grepl("\u2013", lines, fixed = TRUE))){
      emdash_lines <-
        lines %>%
        gsub("\\{[^\\s\\}]+\\}", "\\{\\}", x = ., perl = TRUE) %>%
        grep("(?<!(\\-))--(?!\\-)", x = ., perl = TRUE)
      
      emdash_lines <- union(emdash_lines,
                            grep("\u2013", lines, fixed = TRUE))
      line_no <- emdash_lines[[1]]
      .report_error(line_no = line_no,
                    context = lines[line_no],
                    error_message = "En-dashes not permitted.")
      stop("En-dashes not permitted.")
    }
  }
  
  

  invisible(NULL)
}
