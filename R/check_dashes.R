#' Check dashes entered as hyphens
#'
#' @param filename A tex or Rnw file.
#' @param .report_error How errors should be reported.
#' @return File stops and \code{cat()}s on any line where a hyphen is surrounded by a space.
#' Excludes dashes in knitr chunks and LaTeX math mode \code{\(...\)} but not in TeX math mode \code{$...$}.
#' @export

check_dashes <- function(filename, .report_error){
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }

  lines <- read_lines(filename)

  lines[isR_line_in_knitr(lines)] <- "%"
  if (any(lines == "\\begin{align*}")){
    math_environ <- 
      which(cumsum(lines == "\\begin{align*}") - cumsum(lines == "\\end{align*}") == 1L)
    
    lines[math_environ] <- "% align environment"
  }

  lines <- strip_comments(lines)

  possible_hyphen <- grepl(" - ", lines, fixed = TRUE)

  if (any(possible_hyphen)){
    excluding_mathmode <-
      if_else(possible_hyphen,
              gsub(paste0("\\\\[(]", "[^\\)]*", "\\\\[)]"),
                   "",
                   lines,
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
      .report_error(line_no = line_no,
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
    .report_error(line_no = line_no,
                  context = lines[[line_no]],
                  error_message = "Hyphen adjacent to en-dash.")
    stop("Hyphen adjacent to en-dash. (Did you copy this line from Word?) ",
         "Make sure anything you intend as an en-dash is entered as ' -- '")
  }


  if (any(grepl("\\\\label[^\\}]*\\s[^\\}]*\\}", trimws(lines), perl = TRUE))){
    line_no <- grep("\\\\label[^\\}]*\\s[^\\}]*\\}", trimws(lines), perl = TRUE)[[1]]
    nchars_b4 <- stringi::stri_locate_all_regex(pattern = "\\\\label[^\\}]*\\s", str = trimws(lines[[line_no]]), perl = TRUE)
    context <- paste0(trimws(lines[[line_no]]), "\n",
                      paste0(rep(" ", nchars_b4[[1]][[2]] - 2 + 5 + nchar(line_no)), collapse = ""), "^^")
    .report_error(line_no = line_no,
                  context = context,
                  error_message = "Space somewhere after \\label . Spaces are not permitted in \\label.")
    stop("Space somewhere after \\label. Spaces are not permitted in \\label.")
  }

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

  invisible(NULL)
}
