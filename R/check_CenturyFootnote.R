#' Check location of century footnote
#' @description The formatting of footnote text should be redefined when there are more than 99 footnotes in the document.
#' @param path Directory containing the \strong{aux} file. In particular, \code{pdflatex} must be run before running this function.
#' @param strict Stop if suspected of incorrect placement. Otherwise a note.
#' @return If CenturyFootnote correctly placed, \code{NULL} invisibly. Otherwise, an error.
#' @export 

check_CenturyFootnote <- function(path = ".", strict = FALSE){
  # CRAN NOTE avoidance:
  page <- posx <- column <- NULL
  
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
        .[, lapply(.SD, as.integer), .SDcols = 1:2] %>%
        setkey(fno.)
    }
  
  fno. <- NULL
  footnote_by_page <- 
    grep(paste0("^.*",
                "newlabel\\{footnote@@@[0-9]+\\}", 
                "\\{", 
                # footnote number
                "\\{", 
                "([0-9]+)", 
                "\\}", 
                # 
                "\\{",
                "([0-9]+)", 
                "\\}", 
                ".*$"), aux_contents, perl = TRUE, value = TRUE) %>%
    {
      data.table(
        page = gsub(paste0("^.*",
                           "newlabel\\{footnote@@@[0-9]+\\}", 
                           "\\{", 
                           # footnote number
                           "\\{", 
                           "([0-9]+)", 
                           "\\}", 
                           # 
                           "\\{",
                           "([0-9]+)", 
                           "\\}", 
                           ".*$"),
                    "\\2",
                    .,
                    perl = TRUE),
                    
        fno. = gsub(paste0("^.*",
                           "newlabel\\{footnote@@@[0-9]+\\}", 
                           "\\{", 
                           # footnote number
                           "\\{", 
                           "([0-9]+)", 
                           "\\}", 
                           # 
                           "\\{",
                           "([0-9]+)", 
                           "\\}", 
                           ".*$"),
                    "\\1",
                    .,
                    perl = TRUE)
        )
      } %>%
        .[, lapply(.SD, as.integer), .SDcols = 1:2] %>%
        setkey(fno.)
  
  footnote_by_page_and_postion <- 
    footnote_by_page[footnote_locations]
  
  CenturyFootnote_suspect <- FALSE
  
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
    
    footnote_by_page_and_postion[, column := if_else(posx < page_middle, 1, 2)]
    footnote_by_page_and_postion <- footnote_by_page_and_postion
    
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
          .[, column := dplyr::if_else(posx > page_middle, 2, 1)] %>%
          .[, .(page, column)]
      }
    
    where_should_CenturyFootnote_go <-
      # Find footnote100's position and move to previous column
      # and before that column's footnote
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
          .[, column := dplyr::if_else(posx > page_middle, 1, 2)] %>%
          .[, page := dplyr::if_else(posx > page_middle, page, page - 1L)] %>%
          .[, .(page, column)]
      }
    # list(x = whereis_fn100, page_middle = page_middle)
    if (!identical(where_should_CenturyFootnote_go,
                   whereis_CenturyFootnote)){
      CenturyFootnote_suspect <- TRUE
      if (strict){
        stop("\\CenturyFootnote fell in p.",
             whereis_CenturyFootnote[["page"]], ", column ",
             whereis_CenturyFootnote[["column"]], ". ",
             "It should fall in p.",
             where_should_CenturyFootnote_go[["page"]], ", column ",
             where_should_CenturyFootnote_go[["column"]], ". ")
      } else {
        cat("NOTE: \\CenturyFootnote fell in p.",
            whereis_CenturyFootnote[["page"]], ", column ",
            whereis_CenturyFootnote[["column"]], ". ",
            "I suspect it should have fallen in p.",
            where_should_CenturyFootnote_go[["page"]], ", column ",
            where_should_CenturyFootnote_go[["column"]], ". ",
            "\nI may have been too pendantic, however. ",
            "So visually check the column with the 100th footnote, ",
            "and the column preceding it. If it looks good to you, ", 
            "I was indeed too pedantic, and I apologize.\n", sep = "")
      }
    } else {
      # does it occur after the last footnote in that column?
      prev_column_footnotes <- 
        footnote_by_page_and_postion %>%
        .[and(page == whereis_CenturyFootnote[["page"]],
              column == whereis_CenturyFootnote[["column"]])]
      
      if (nrow(prev_column_footnotes) > 0){
        last_footnote_no <- 
          prev_column_footnotes %>%
          last %>%
          .[["fno."]]
        
        CenturyFootnote_written_after <- 
          scan(dir(path = path, pattern = "\\.fn100$", full.names = TRUE)[[1]], 
               sep = "\n", 
               quiet = TRUE) %>%
          last
        
        if (last_footnote_no != CenturyFootnote_written_after){
          stop("\\CenturyFootnote in correct column but needs to be placed after that column's last footnote: ", 
               last_footnote_no)
        }
      }
    }
  }
  
  assign("CenturyFootnote_suspect", CenturyFootnote_suspect, pos = parent.frame())
  invisible(NULL) 
}

