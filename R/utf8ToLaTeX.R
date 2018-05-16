#' Translate UTF-8 to ASCII with LaTeX
#' @description Attempts to convert a UTF-8 file to one where all non-ASCII characters are replaced by control sequences which resolve in LaTeX or BibTeX to the equivalent printed characters. Inteded to be used when a bibliography needs to be fed to BibTeX (which does not support UTF-8).
#' @param input The text file containing UTF-8 to read
#' @param filename,text May be provided instead of \code{input}.
#' @param outfile Where to direct the result. By default, the console unless \code{filename} is provided or \code{input} resolves to a filename, in which case the filename again (i.e. the filename is overwritten with the result).
#' @param rstudio In case of a problem during the conversion, use the RStudio API to pop to the location in the filename. Only applicable if \code{input} resolves to a file.
#' @param verbose Be chatty and report progress?
#' @return The filename with new contents
#' 

utf8ToLaTeX <- function(input, filename = NULL, text = NULL, outfile = filename, rstudio = FALSE, verbose = FALSE) {
  input_is_file <- 
    if (is.null(filename)) {
      length(input) == 1L && file.exists(input)
    } else {
      file.exists(filename)
    }
  if (verbose) {
    cat("input_is_file: \n")
  }
  
  if (is.null(filename) && is.null(text)) {
    if (length(input) == 1L && file.exists(input)) {
      tex_lines <- 
        if (requireNamespace("readr", quietly = TRUE)) {
          readr::read_lines(input)
        } else {
          readLines(input, encoding = "UTF-8")
        }
    } else {
      tex_lines <- input
    }
  } else if (is.null(text)) { 
    tex_lines <- 
      if (requireNamespace("readr", quietly = TRUE)) {
        readr::read_lines(filename)
      } else {
        readLines(filename, encoding = "UTF-8")
      }
  } else {
    tex_lines <- text
  }
  nchar_tex_lines <- nchar(tex_lines)
  n_char <- sum(nchar_tex_lines)
  input_dt <-
    setDT(list(char_no = seq_len(n_char),
               line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
               column = unlist(lapply(nchar_tex_lines, seq_len), use.names = FALSE),
               char = unlist(strsplit(tex_lines,
                                      split = "",
                                      fixed = TRUE),
                             use.names = FALSE,
                             recursive = FALSE)))
  setattr(input_dt, "sorted", "char_no")
  
  ok <- c(" ",
          "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", 
          "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", 
          "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", 
          "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", 
          "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d", 
          "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", 
          "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~")
  tryCatch(input_dt[char %notin% ok, char := convert_chars_to_LaTeX(char)],
           error = function(e) {
             m <- e$m
             if (verbose) cat(m, "\n")
             for (i in input_dt[char %notin% ok][["char_no"]]) {
               if (verbose) cat(i, "\n")
               tryCatch(convert_char_to_LaTeX(input_dt[i][["char"]]), 
                        error = function(e) {
                          report2console(file = if (input_is_file) input %||% filename, 
                                         line_no = input_dt[i][["line_no"]],
                                         column = input_dt[i][["column"]],
                                         error_message = paste0("Problem with UTF-8 conversion:\n", 
                                                                e$m),
                                         advice = "Likely treatment: delete this char.", 
                                         rstudio = input_is_file && rstudio)
                          stop("Problem with UTF-8 conversion", call. = FALSE)
                          break
                        })
             }
           })
  ans <- input_dt[, .(text = paste0(char, collapse = "")), keyby = "line_no"]
  all_line_nos <- setDT(list(line_no = seq_along(tex_lines)))
  
  if (!is.null(outfile)) {
    writeLines(ans[all_line_nos, on = "line_no"][is.na(text), text := ""][["text"]], 
               outfile)
  } else {
    ans[all_line_nos, on = "line_no"][is.na(text), text := ""][["text"]]
  }
}

convert_chars_to_LaTeX <- function(chars, strict = TRUE) {
  vapply(chars, convert_char_to_LaTeX, "", strict = strict)
}

convert_char_to_LaTeX <- function(char, strict = TRUE) {
  utf8 <- enc2utf8(char)
  int <- utf8ToInt(utf8)
  if (int < 160L) {
    char
  } else {
    switch(as.character(int),
           "160" = "~",
           "163" = "\\pounds", 
           "224" = "\\`{a}", 
           "225" = "\\'{a}", 
           "228" = '\\"{a}', 
           "231" = "\\c{c}", 
           "232" = "\\`{e}",
           "233" = "\\'{e}",
           "237" = "\\'{\\i}",
           "243" = "\\'{o}", 
           "246" = '\\"{o}',
           "248" = "\\o", 
           "252" = '\\"{u}',
           "321" = "\\L",
           "353" = "\\v{s}",
           "8206" = if (strict) stop("Detected utf8 codepoint 8206 (LEFT TO RIGHT SYMBOL)."),
           "8208" = "-",
           "8211" = "--",
           "8212" = "---",
           "8216" = "`",
           "8217" = "'",
           "8220" = "``",
           "8221" = "''",
           if (strict) stop("Found character with UTF-8:\n", utf8, "\n",
                            "U", int))
  }
}






