#' @name TexCheckR-package
#' @title TexCheckR
#' @description Checks LaTeX documents and .bib files for typing errors, such as spelling errors, incorrect quotation marks. Also provides useful functions for parsing and linting bibliography files.  
#'
#' @import data.table
#' 
#' @importFrom clisymbols symbol
#' @importFrom crayon green red bgGreen bgRed bold
#' @importFrom fastmatch %fin%
#' @importFrom grDevices embedFonts
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @importFrom magrittr %>%
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom magrittr not
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils %notin%
#' @importFrom hutils neither
#' @importFrom hutils duplicated_rows
#' @importFrom readr read_lines
#' @importFrom stats complete.cases
#' @importFrom stringi stri_sub
#' @importFrom utils download.file
NULL

