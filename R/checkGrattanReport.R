#' Check Grattan Report
#'
#' @param path Path to search for the tex source file.
#' @param output_method How errors should be reported.
#' @param compile Should \code{pdflatex} be run on the report so the log be checked?
#' @param final Should the document be assumed to be final? Runs additional checks.
#' @param release Should a final pdf be prepared for publication?
#' @return Called for its side-effect.
#' @export
#' @importFrom magrittr %>%
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom magrittr not
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom clisymbols symbol
#' @importFrom crayon green red bgGreen bgRed

checkGrattanReport <- function(path = ".",
                               output_method = c("console", "twitter", "gmailr"),
                               compile = FALSE,
                               final = FALSE,
                               release = FALSE){
  if (!identical(release, FALSE)){
    stop("Release not implemented.")
  }
  
  if (release && (!final || !compile)){
    stop("release = TRUE but final and compile are not both TRUE also.")
  }
  
  if (final && !compile){
    stop("final = TRUE but compile = FALSE.")
  }
  
  if (compile && Sys.which("pdflatex") == ""){
    stop("pdflatex not on System path. Ensure you have LaTeX installed (MiKTeX, MacTeX, TeXLive) and that it is searchable on PATH. ",
         "(Did you install but leave programs open?)")
  }
  
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))
  
  output_method <- match.arg(output_method)
  
  if (!dir.exists("./travis/grattanReport/")){
    stop("./travis/grattanReport/ does not exist.")
  }

  tex_file <- dir(path = ".", pattern = "\\.tex$")
  if (length(tex_file) != 1L){
    stop("path must contain one and only one .tex file.")
  }
  filename <- tex_file[[1]]

  the_authors <-
    get_authors(filename)

  cat("I see the following as authors:",
      the_authors, sep = "\n")

  .report_error <- function(...){
    report2console(...)
  }
  
  report_name <- gsub("^(.*)\\.tex$", "\\1", tex_file)

  switch(output_method, 
         "twitter" = {
           stopifnot(file.exists("~/twitteR/grattan-reporter.R"))
           source("~/twitteR/grattan-reporter.R")
           twitter_handle <- name <- NULL
           authors_twitter_handles <-
             Grattan_staff %>%
             .[and(name %in% the_authors,
                   nchar(twitter_handle) > 0)] %>%
             .[["twitter_handle"]] %>%
             paste0("@", .)
           
           .report_error <- function(...){
             report2twitter(...,
                            authors = authors_twitter_handles,
                            build_status = "Broken:",
                            report_name = report_name)
           }
         }, 
         "gmailr" = {
           .report_error <- function(...){
             report2gmail(...,
                          report_name = report_name, 
                          authors = the_authors)
           }
         })
  
  cat("\n")
  
  
  
  check_input <- function(filename){
    inputs <- inputs_of(filename)
    if (length(inputs) > 0){
      for (input in inputs){
        check_input(input)
        cat(input, "\n")
      
        check_cite_pagerefs(input, .report_error = .report_error)
        check_dashes(input)#, .report_error = .report_error)
        check_footnote_typography(input)#, .report_error = .report_error)
        check_repetitive_xrefs(input)#, .report_error = .report_error)
        check_sentence_ending_periods(input)#, .report_error = .report_error)
      }
    }
  }
  check_input(filename)
  
  check_cite_pagerefs(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Cite and pagerefs checked.\n"), sep = "")


  check_dashes(filename)
  cat(green(symbol$tick, "Dashes correctly typed.\n"))

  check_footnote_typography(filename)
  cat(green(symbol$tick, "Footnote typography checked.\n"))
  
  check_repetitive_xrefs(filename)
  cat(green(symbol$tick, "No repetitive xrefs.\n"))

  check_sentence_ending_periods(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Sentence-ending periods ok.\n"))

  check_spelling(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Spellcheck complete.\n"))

  # To check the bibliography
  bib_files <-
    readLines(filename, warn = FALSE) %>%
    .[grepl("\\addbibresource", ., fixed = TRUE)] %>%
    trimws %>%
    gsub("^\\\\addbibresource[{](.+\\.bib)[}]$", "\\1", .)
  for (bib_file in bib_files){
    validate_bibliography(file = bib_file)
  }

  cat(green(symbol$tick, "Bibliography validated.\n"))

  check_labels(filename)

  cat(green(symbol$tick, "Labels checked.\n"))

  check_all_figs_tbls_refd(filename)

  cat(green(symbol$tick, "All figures and tables have a Xref.\n"))

  cat("\n")
  
  if (compile){
    cat("Invoking pdflatex\n")
    # move_dir <- function(to.dir, from.dir = "."){
    #   x <- list.files(path = from.dir,
    #                   full.names = TRUE,
    #                   recursive = TRUE,
    #                   include.dirs = TRUE)
    #   dests <- file.path(to.dir, x)
    #   file.copy(x, dests)
    # }
    # 
    # move_dir(tempdir())
    
    options(warn = 2)
    on.exit({
      if (file.exists(gsub("\\.tex$", ".log2", filename))){
        file.remove(gsub("\\.tex$", ".log2", filename))
      }})
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    
    system2(command = "biber",
            args = c("--onlylog", "-V", gsub("\\.tex$", "", filename)),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    check_biber()
    cat(green(symbol$tick, "biber validated citations.\n"))
    
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    system2(command = "pdflatex",
            args = c("-interaction=batchmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    check_log()
    cat(green(symbol$tick, ".log file checked.\n"))
    
    check_CenturyFootnote()
    cat(green(symbol$tick, "\\CenturyFootnote correctly placed.\n"))
    cat("\n")
  }
  
  cat(bgGreen(symbol$tick, "Report checked.\n"))
  
  if (output_method == "gmailr"){
    if (file.exists("./travis/grattanReport/gmailr-log.tsv")){
      prev_build_status <-
        fread("./travis/grattanReport/gmailr-log.tsv") %>%
        utils::tail(., 1) %>%
        .[["build_status"]]
      append <- TRUE
    } else {
      prev_build_status <- "None"
      append <- FALSE
    }
    
    if (prev_build_status %in% c("None", "Broken", "Still failing")){
      message <- gmailr::mime(
        To = "hugh.parsonage@gmail.com", #email_addresses, 
        From = "hugh.parsonage@gmail.com",
        Subject = paste0("Fixed: ", report_name)
      ) %>%
        gmailr::html_body(body = paste0(c("grattanReporter returned no error.")))
      gmailr::send_message(message)
    }
    
    data.table(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               build_status = "OK", 
               error_message = "") %>%
      fwrite("./travis/grattanReport/gmailr-log.tsv",
             sep = "\t",
             append = append)
  }
}
