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
        stop("`words_ante = ", deparse(substitute(words_ante)), "` was type ", typeof(words_ante), ", ",
             "but must be an integer.")
      }
      
      if (length(words_ante) != 1L) {
        stop("`words_ante = ", deparse(substitute(words_ante)), "` had length ", length(words_ante), ", ",
             "but must be length-one.")
      }
      
      if (words_ante <= 0L) {
        stop("`words_ante = ", deparse(substitute(words_ante)), "` was not a positive integer.")
      }
    }
    
    if (quote || sentence) {
      # Replace with white space
      out[i] <-
        
        # Add space to invoke split regex
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
    } else {
      
      # word
      words_ante_i <- words_ante
      
      if (stri_count_fixed(sic_line, "[sic]") > 1L) {
        sic_line_split <- strsplit(sic_line, split = "(?<=(?:\\[sic\\]))\\W", perl = TRUE)[[1L]]
        out[i] <- paste0(vapply(sic_line_split, 
                                function(x) {
                                  replace_words_before(anchor = "\\[sic\\]",
                                                       text = x,
                                                       n_words = words_ante)
                                },
                                FUN.VALUE = character(1L)), 
                         collapse = " ")
      } else {
        out[i] <- replace_words_before(anchor = "\\[sic\\]", sic_line, n_words = words_ante)
      }
        
      
    }
  }
  
  out
}

replace_words_before <- function(anchor, text, n_words) {
  words_ante_i <- n_words
  out <- text
  
  regex_text <- paste0("\\W*\\w+\\W+(?=", anchor, ")")
  
  while (words_ante_i > 0L &&
         grepl(sprintf("\\W*\\w+\\W+%s", anchor), out, perl = TRUE)) {
    nchar_before_sic <- attr(regexpr(regex_text, out, perl = TRUE),
                             "match.length")
    out <- sub(regex_text,
               formatC(" ", width = nchar_before_sic),
               out,
               perl = TRUE)
    words_ante_i <- words_ante_i - 1L
  }
  nchar_anchor <- attr(regexpr(anchor, out, perl = TRUE),
                       "match.length")
  out <- sub(anchor, formatC(" ", width = nchar_anchor), out, perl = TRUE)
  
  out
}




