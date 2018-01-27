#' Veto sic 
#' 
#' @description Vetoes words in a LaTeX document that are marked '[sic]' 
#' for the purpose of spell checking by replacing them (and `[sic]` itself) with white space of 
#' equal length.
#' 
#' @param tex_lines A character vector.
#' @param quote (logical, default: \code{TRUE}) Veto words after the previous opening quote (\emph{i.e.} backtick) symbol.
#' @param sentence (logical, default: \code{TRUE}) Veto words before [sic] in the same sentence.
#'  (The start of a sentence is taken to be the location of the capital letter which is preceded 
#'  by white space and a full stop.)
#' @param words_ante The number of words to exclude. Ignored if \code{quote} or \code{sentence} is \code{TRUE}.
#' @export veto_sic


veto_sic <- function(tex_lines, quote = TRUE, sentence = !quote, words_ante = 1L) {
  out <- tex_lines
  sic_line_nos <- grep("[sic]", tex_lines, fixed = TRUE)
  
  for (i in sic_line_nos) {
    sic_line <- tex_lines[i]
    
    if (quote) {
      split_regex = "`"
    } else if (sentence) {
      split_regex = "(?<=\\.)\\s+(?=[A-Z])"
    } else {
      if (!is.integer(words_ante)) {
        stop("`words_ante = ", deparse(substitute(words_ante)), "` was type ", type(words_ante), ", ",
             "but must be an integer.")
      }
      
      if (length(words_ante) != 1L) {
        stop("`words_ante = ", deparse(substitute(words_ante)), "` had length ", length(words_ante), ", ",
             "but must be length-one.")
      }
      
      if (words_ante <= 0L) {
        stop("`words_ante = ", deparse(subtitute(words_ante)), "` was not a positive integer.")
      }
      
      if (words_ante == 1L) {
        split_regex <- "\\s(?=([[:punct:]]*(\\w+)[[:punct:]]*)\\s\\[sic\\])"
      } else {
        split_regex <- sprintf("\\s(?=([[:punct:]]*\\w+(\\b(\\s|\\b)?\\w+[[:punct:]]*){1,%d})\\s\\[sic\\])", words_ante - 1L)
      }
    }
    # Replace with white space
    out[i] <-
      paste0(vapply(strsplit(sic_line, split = split_regex, perl = TRUE)[[1L]],
                    function(x) {
                      nchar_before_sic <- attr(regexpr("^.*\\[sic\\]", x, perl = TRUE), "match.length")
                      # technically == 0L means a match (for some reason [sic] is at beginning of sentence),
                      # but no actual impact so just use that.
                      if (nchar_before_sic > 0L) {
                        sub("^.*\\[sic\\]", formatC(" ", width = nchar_before_sic), x, perl = TRUE)
                      } else {
                        x
                      }
                    },
                    character(1L)),
             # Issue: multiple spaces between sentences?
             collapse = " ")
    
  }
  
  out
}



