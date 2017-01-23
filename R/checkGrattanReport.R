#' Check Grattan Report
#' @description Check Grattan reports written is LaTeX for typing errors, significant warnings, 
#' and inconsistent style.
#' @param path Path to search for the tex source file.
#' @param output_method How errors should be reported.
#' @param compile Should \code{pdflatex} be run on the report so the logs may be checked?
#' @param final Should the document be assumed to be final? Runs additional checks.
#' @param release Should a final pdf be prepared for publication?
#' @param .proceed_after_rerun On the occasions where infinitely many passes of \code{pdflatex} 
#' are required, include this to skip the error. Note that this will result in false cross-references 
#' or incompletely formatted bibliographies.
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
#' @importFrom grDevices embedFonts
#' @importFrom utils download.file

checkGrattanReport <- function(path = ".",
                               output_method = c("console", "twitter", "gmailr"),
                               compile = FALSE,
                               final = FALSE,
                               release = FALSE,
                               .proceed_after_rerun){
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
  
  if (release && Sys.getenv("R_GSCMD") == ""){
    stop("Ghostscript is required but R_GSCMD is not set. Ensure Ghostscript is installed then set R_GSCMD, e.g.\n\t",
         "Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')")
  }
  
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))
  
  output_method <- match.arg(output_method)
  
  if (final){
    download_failure <- download.file("https://raw.githubusercontent.com/HughParsonage/grattex/master/grattan.cls",
                                      destfile = "grattan.cls",
                                      quiet = TRUE)
    if (download_failure){
      stop("grattan.cls failed to download from master branch (and be updated).")
    }
  }
  
  if (!dir.exists("./travis/grattanReport/")){
    stop("./travis/grattanReport/ does not exist.")
  }
  
  if (release && !dir.exists("RELEASE")){
    warning("release = TRUE but there is no existing RELEASE folder so one will be created.")
    dir.create("RELEASE")
  }

  tex_file <- dir(path = ".", pattern = "\\.tex$")
  if (length(tex_file) != 1L){
    stop("path must contain one and only one .tex file.")
  }
  filename <- tex_file[[1]]
  
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
  
  check_preamble(filename, .report_error, final = final, release = release)

  the_authors <-
    get_authors(filename)

  cat("I see the following as authors:",
      the_authors, sep = "\n   ")
  
  cat("\n")
  
  check_input <- function(filename){
    inputs <- inputs_of(filename)
    if (length(inputs) > 0){
      for (input in inputs){
        check_input(input)
        cat(input)
      
        check_cite_pagerefs(input, .report_error = .report_error)
        cat(".")
        check_escapes(input, .report_error = .report_error)
        cat(".")
        check_dashes(input, .report_error = .report_error)
        cat(".")
        check_quote_marks(input, .report_error = .report_error)
        cat(".")
        check_footnote_typography(input, .report_error = .report_error)
        cat(".")
        check_repetitive_xrefs(input, .report_error = .report_error)
        cat(".")
        check_sentence_ending_periods(input, .report_error = .report_error)
        cat(".")
        cat(" OK\n")
      }
    }
  }
  check_input(filename)
  
  check_cite_pagerefs(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Cite and pagerefs checked.\n"), sep = "")

  check_escapes(filename)
  cat(green(symbol$tick, "No unescaped $.\n"))
  
  check_dashes(filename)
  cat(green(symbol$tick, "Dashes correctly typed.\n"))
  
  check_quote_marks(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Opening quotes correctly typed.\n"))

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
    cat(green(symbol$tick, bib_file, "validated.\n"))
  }

  check_labels(filename)

  cat(green(symbol$tick, "Labels checked.\n"))

  check_all_figs_tbls_refd(filename)

  cat(green(symbol$tick, "All figures and tables have a Xref.\n"))

  cat("\n")
  
  if (compile){
    full_dir_of_path <- getwd()
    move_to <- function(to.dir, from.dir = "."){
      x <- list.files(path = from.dir,
                      pattern = "\\.((pdf)|(tex)|(cls)|(sty)|(Rnw)|(bib)|(png)|(jpg))$",
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = FALSE)
      x.dirs <- file.path(to.dir, 
                          list.dirs(path = from.dir, recursive = TRUE, full.names = TRUE))
      dir_create <- function(x) if (!dir.exists(x)) dir.create(x)
      lapply(x.dirs, dir_create)
      file.copy(x, file.path(to.dir, x), overwrite = TRUE, recursive = FALSE)
      setwd(to.dir)
      file.remove(gsub("\\.tex", ".pdf", filename))
      cat("Attempting compilation in temp directory:", to.dir, "\n")
    }
    md5_filename <- tools::md5sum(filename)
    temp_dir <- file.path(tempdir(), md5_filename)
    md5_iter <- 1
    while (dir.exists(temp_dir)){
      md5_filename <- gsub("^(.)(.+)$", "\\2\\1", md5_filename)
      temp_dir <- file.path(tempdir(), md5_filename)
      md5_iter <- md5_iter + 1
      if (md5_iter > 30){
        cat(tempdir())
        stop("Emergency stop: temporary directory full.")
      }
    }
    dir.create(temp_dir)
    move_to(temp_dir)
    
    cat("Invoking pdflatex... ")
    options(warn = 2)
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    cat("complete.\n")
    cat("Invoking biber...\n")
    system2(command = "biber",
            args = c("--onlylog", "-V", gsub("\\.tex$", "", filename)),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    check_biber()
    cat(green(symbol$tick, "biber validated citations.\n"))
    
    cat("Rerunning pdflatex. Starting pass number 1")
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    cat(" 2 ")
    system2(command = "pdflatex",
            args = c("-interaction=batchmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    log_result <- check_log(check_for_rerun_only = TRUE)
    reruns_required <- 2
    while (final && !is.null(log_result) && log_result == "Rerun LaTeX."){
      cat(" ", reruns_required + 1, " ", sep = "")
      system2(command = "pdflatex",
              args = c("-interaction=batchmode", filename),
              stdout = gsub("\\.tex$", ".log2", filename))
      log_result <- check_log(check_for_rerun_only = TRUE)
      
      reruns_required <- reruns_required + 1
      if (!missing(.proceed_after_rerun) && reruns_required > .proceed_after_rerun){
        cat("\nW: Skipping checking of LaTeX rerun.")
        break
      }
      
      if (missing(.proceed_after_rerun) && reruns_required > 9){
          stop("Emergency stop: pdflatex had to rerun more than 9 times but could not stabilize cross-references or the bibliography. ",
               "Consult an expert: Hugh Parsonage or Cameron Chisholm or https://tex.stackexchange.com.")
      }
    }
    cat("\n")
    cat(green(symbol$tick, ".log file checked.\n"))
    
    if (final){
      check_CenturyFootnote()
      cat(green(symbol$tick, "\\CenturyFootnote correctly placed.\n"))
      
      if (release){
        if (!dir.exists("RELEASE")){
          dir.create("RELEASE")
        }
        # Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
        embedFonts(gsub("\\.tex$", ".pdf", filename),
                   outfile = file.path(full_dir_of_path, "RELEASE", gsub("\\.tex$", ".pdf", filename)))
        cat(green(symbol$tick, "Fonts embedded.\n"))
        
      }
      
      setwd(full_dir_of_path)
      cat("\n")
    }
  }
  
  cat(bgGreen(symbol$tick, "Report checked.\n"))
  if (release){
    cat("Releaseable pdf written to ", file.path(path, "RELEASE", gsub("\\.tex$", ".pdf", filename)))
    cat("\nDONE.")
  }
  
  if (output_method == "gmailr"){
    if (file.exists("./travis/grattanReport/gmailr-log.tsv")){
      prev_build_status <-
        fread("./travis/grattanReport/gmailr-log.tsv") %>%
        last %>%
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
