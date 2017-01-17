#' Check location of century footnote
#' @description The formatting of footnote text should be redefined when there are more than 99 footnotes in the document.
#' @param path Directory containing the \strong{aux} file. In particular, \code{pdflatex} must be run before running this function.
#' @return If CenturyFootnote correctly placed, \code{NULL} invisibly. Otherwise, an error.
#' @export 

check_CenturyFootnote <- function(path = "."){
  aux_file <- dir(path = path, pattern = "\\.aux$", full.names = TRUE)
  
  if (length(aux_file) == 0L){
    stop("There is no .aux file in 'path'.\npdflatex must be run before this check can complete.")
  }
  
  if (length(aux_file) > 1L){
    stop("Multiple .aux files in 'path'.")
  }
  
  aux_contents <- readLines(aux_file)
  
  footnote_locations <-
    grep("zref@newlabel{footnote@@@", aux_contents, fixed = TRUE, value = TRUE) %>%
    {
      data.table(
        posx = gsub("^.*posx[{]([0-9]+)[}].*$", "\\1", x = ., perl = TRUE),
        fno. = gsub("^.zref@newlabel[{]footnote@@@([0-9]+)[}].*$", "\\1", x = ., perl = TRUE)
      ) %>%
        .[, lapply(.SD, as.integer), .SDcols = 1:2]
    }
  
  if (any(footnote_locations[["fno."]] >= 100) || 
      any(grepl("CenturyFootnote", aux_contents, fixed = TRUE))){
    
    if (!any(grepl("CenturyFootnote", aux_contents, fixed = TRUE))){
      stop("\\CenturyFootnote was not used, but the number of footnotes exceeds 99.")
    }
    
    if (!any(footnote_locations[["fno."]] >= 100)){
      stop("\\CenturyFootnote was used but there are fewer than 100 footnotes.")
    }
    
    fn_before_CenturyFootnote <-
      gsub("^.*CenturyFootnote@@@([0-9]+)@cref.*",
           "\\1",
           grep("^.*CenturyFootnote@@@([0-9]+)@cref.*",
                aux_contents, 
                perl = TRUE,
                value = TRUE),
           perl = TRUE) %>%
      as.integer
    
    if (fn_before_CenturyFootnote >= 100){
      stop("CenturyFootnote occurs after the 100th footnote.")
    }
    
    page_middle <-
      footnote_locations %>%
      .[["posx"]] %>%
      mean
    
    whereis_CenturyFootnote <-
      grep("newlabel{@CenturyFootnote", aux_contents, fixed = TRUE, value = TRUE) %>%
      {
        data.table(
          page = gsub("^.*@cref.*[{]([0-9]+)[}][}]$",
                      "\\1", 
                      grep("@cref", x = ., fixed = TRUE, value = TRUE), 
                      perl = TRUE),
          posx = gsub("^.*posx[{]([0-9]+)[}].*$",
                      "\\1",
                      grep("zref@", x = ., fixed = TRUE, value = TRUE),
                      perl = TRUE)
        ) %>%
          .[, lapply(.SD, as.numeric), .SDcols = 1:2] %>%
          .[, column := dplyr::if_else(posx > page_middle, "Right", "Left")] %>%
          .[, .(page, column)]
      }
    
    where_should_CenturyFootnote_go <-
      # Find footnote100's position and move to previous column
      grep("newlabel{footnote@@@100", aux_contents, fixed = TRUE, value = TRUE) %>%
      {
        data.table(
          page = gsub("^.*@cref.*[{]([0-9]+)[}][}]$",
                      "\\1", 
                      grep("@cref", x = ., fixed = TRUE, value = TRUE), 
                      perl = TRUE),
          posx = gsub("^.*posx[{]([0-9]+)[}].*$",
                      "\\1",
                      grep("zref@", x = ., fixed = TRUE, value = TRUE),
                      perl = TRUE)
        ) %>%
          .[, lapply(.SD, as.numeric), .SDcols = 1:2] %>%
          .[, column := dplyr::if_else(posx > page_middle, "Left", "Right")] %>%
          .[, page := dplyr::if_else(posx > page_middle, page, page - 1L)] %>%
          .[, .(page, column)]
      }
    # list(x = whereis_fn100, page_middle = page_middle)
    if (!identical(where_should_CenturyFootnote_go,
                   whereis_CenturyFootnote)){
      stop("\\CenturyFootnote fell in p.",
           whereis_CenturyFootnote[["page"]], ", ",
           tolower(whereis_CenturyFootnote[["column"]]), " column. ",
           "It should fall in p.",
           where_should_CenturyFootnote_go[["page"]], ", ",
           tolower(where_should_CenturyFootnote_go[["column"]]), " column. ")
    }
  }
  invisible(NULL) 
  
}

