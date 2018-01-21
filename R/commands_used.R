#' List all unique commands in a document
#' @param tex_lines A LaTeX document as read from \code{readr::read_lines} or \code{readLines}.
#' @return A character vector of unique commands used in \code{tex_lines}.
#' 
#' @examples 
#' commands_used(c("A \\abc{d}", "\\def{x}"))
#' 
#' @export

commands_used <- function(tex_lines) {
  words <- unlist(strsplit(tex_lines, split = "\\s", perl = TRUE))
  candidate_commands <- 
    unlist(strsplit(unique(grep("^\\\\", words, perl = TRUE, value = TRUE)),
             split = "(?<=.)(?=\\\\)",
             perl = TRUE))
  
  candidate_commands <-
    unlist(strsplit(candidate_commands,
                    split = "\\{|\\[|[*]|[0-9]|[~]|\\}",
                    perl = TRUE))
  
  candidate_commands <- 
    grep("^\\\\", 
         candidate_commands,
         perl = TRUE, 
         value = TRUE)
  
  candidate_commands <- gsub("[^A-Za-z]$", "", candidate_commands, perl = TRUE)
  
  unique(candidate_commands)
}




