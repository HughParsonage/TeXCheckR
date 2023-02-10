#' @name TeXCheckR-package
#' @title TeXCheckR
#' @description Checks LaTeX documents and \code{.bib} files for typing errors, such as spelling errors, incorrect quotation marks. Also provides useful functions for parsing and linting bibliography files.  
#'
#' @rawNamespace import(data.table, except="%notin%")
#' 
#' @importFrom clisymbols symbol
#' @importFrom crayon green red bgGreen bgRed bold
#' @importFrom fastmatch %fin%
#' @importFrom grDevices embedFonts
#' @importFrom hunspell hunspell
#' @importFrom hunspell dictionary
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom magrittr not
#' @importFrom magrittr subtract
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils %notin%
#' @importFrom hutils NOR
#' @importFrom hutils XOR
#' @importFrom hutils neither
#' @importFrom hutils duplicated_rows
#' @importFrom stats complete.cases
#' @importFrom utils download.file
#' @importFrom utils head
NULL

