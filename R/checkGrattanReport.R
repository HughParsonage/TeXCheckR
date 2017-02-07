#' Check Grattan Report
#' @description Check Grattan reports written is LaTeX for typing errors, significant warnings, 
#' and inconsistent style.
#' @param path Path to search for the tex source file.
#' @param output_method How errors should be reported.
#' @param compile Should \code{pdflatex} be run on the report so the logs may be checked?
#' @param pre_release Should the document be assumed to be final? Runs additional checks.
#' @param release Should a final pdf be prepared for publication?
#' @param .proceed_after_rerun On the occasions where infinitely many passes of \code{pdflatex} 
#' are required, include this to skip the error. Note that this will result in false cross-references 
#' or incompletely formatted bibliographies.
#' @param .no_log Make no entry in the log file on the check's outcome.
#' @param embed If \code{FALSE}, not attempt to embed the fonts using Ghostscript is attempted. Useful if Ghostscript cannot easily be installed.
#' Set to \code{TRUE} for debugging or repetitive use (as in benchmarking). 
#' @param update_grattan.cls Download \code{grattan.cls} from \url{https://github.com/HughParsonage/grattex/blob/master/grattan.cls}? 
#' Set to \code{FALSE} when checking the \code{grattex} repo itself.
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
#' @importFrom crayon green red bgGreen bgRed bold
#' @importFrom grDevices embedFonts
#' @importFrom utils download.file
#' @importFrom stats complete.cases
#' @importFrom readr read_lines

checkGrattanReport <- function(path = ".",
                               output_method = c("console", "twitter", "gmailr"),
                               compile = FALSE,
                               pre_release = FALSE,
                               release = FALSE,
                               .proceed_after_rerun,
                               .no_log = FALSE, 
                               embed = TRUE, 
                               update_grattan.cls = TRUE){
  if (release && (!pre_release || !compile)){
    stop("release = TRUE but pre_release and compile are not both TRUE also.")
  }
  
  if (pre_release && !compile){
    stop("pre_release = TRUE but compile = FALSE.")
  }
  
  if (compile && Sys.which("pdflatex") == ""){
    stop("pdflatex not on system path. Ensure you have LaTeX installed (MiKTeX, MacTeX, TeXLive) and that it is searchable on PATH. ",
         "(Did you install but leave programs open?)")
  }
  
  if (embed && release && Sys.getenv("R_GSCMD") == ""){
    stop("Ghostscript is required but R_GSCMD is not set. Ensure Ghostscript is installed then set R_GSCMD, e.g.\n\t",
         "Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')")
  }
  
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))
  
  output_method <- match.arg(output_method)
  
  if (pre_release && update_grattan.cls && !identical(tolower(Sys.getenv("TRAVIS_REPO_SLUG")), "hughparsonage/grattex")){
    download_failure <- download.file("https://raw.githubusercontent.com/HughParsonage/grattex/master/grattan.cls",
                                      destfile = "grattan.cls",
                                      quiet = TRUE)
    if (download_failure){
      stop("grattan.cls failed to download from master branch (and be updated).")
    }
  }
  
  if (!dir.exists("./travis/grattanReport/")){
    stop("./travis/grattanReport/ does not exist. Create this directory and try again.")
  }
  
  if (release){
    if (!dir.exists("RELEASE")){
      dir.create("RELEASE")
    } else {
      if (length(list.files(path = "./RELEASE", pattern = "\\.pdf$")) > 0){
        # If there are any files in the RELEASE directory, move them (file.rename)
        # to a subdirectory named by their creation time.
        invisible({
          lapply(list.files(path = "./RELEASE", pattern = "\\.pdf$", full.names = TRUE), 
                 function(file){
                   date_created <- format(file.info(file)$ctime, format = "%Y-%m-%d-%H%M")
                   if (!dir.exists(file.path("RELEASE", date_created))){
                     dir.create(file.path("RELEASE", date_created))
                   }
                   file.rename(file, file.path("RELEASE", date_created, basename(file)))
                 })
        })
        message("RELEASE contained pdf files. These have been moved.")
      }
    }
  }
  
  if (pre_release){
    if (!dir.exists("PRE-RELEASE")){
      dir.create("PRE-RELEASE")
    } else {
      if (length(list.files(path = "./PRE-RELEASE", pattern = "\\.pdf$")) > 0){
        invisible(lapply(list.files(path = "./PRE-RELEASE", pattern = "\\.pdf$", full.names = TRUE), file.remove))
        message("PRE-RELEASE contained pdf files. These have been deleted.")
      }
    }
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
  
  # Actual checking begins here
  notes <- 0L
  
  
  check_preamble(filename, .report_error, pre_release = pre_release, release = release)

  the_authors <-
    get_authors(filename)
  
  if (length(the_authors) == 0L){
    stop("No authors detectable in document.")
  }

  cat("I see the following as authors:",
      the_authors, sep = "\n   ")
  
  cat("\n")
  
  cat(green(symbol$tick, "Preamble OK.\n"), sep = "")
  
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
        check_spacing(input, .report_error = .report_error)
        cat(".")
        check_quote_marks(input, .report_error = .report_error)
        cat(".")
        check_footnote_typography(input, .report_error = .report_error)
        cat(".")
        check_labels(input)
        cat(".")
        check_literal_xrefs(input, .report_error = .report_error)
        cat(".")
        check_xrefs(input, .report_error = .report_error)
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
  
  check_spacing(filename, .report_error = .report_error)
  cat(green(symbol$tick, "No spacing issues around abbreviations.\n"))
  
  check_quote_marks(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Opening quotes correctly typed.\n"))

  check_footnote_typography(filename)
  cat(green(symbol$tick, "Footnote typography checked.\n"))
  
  check_literal_xrefs(filename, .report_error = .report_error)
  check_xrefs(filename)
  cat(green(symbol$tick, "No repetitive xrefs.\n"))

  check_sentence_ending_periods(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Sentence-ending periods ok.\n"))
  
  # To check the bibliography
  bib_files <-
    read_lines(filename) %>%
    .[grepl("\\addbibresource", ., fixed = TRUE)] %>%
    trimws %>%
    gsub("^\\\\addbibresource[{](.+\\.bib)[}]$", "\\1", .)
  
  for (bib_file in bib_files){
    validate_bibliography(file = bib_file)
    cat(green(symbol$tick, bib_file, "validated.\n"))
  }
  
  any_bib_duplicates(bib.files = bib_files)
  cat(green(symbol$tick, "No obvious duplicates in bibliography.\n"))

  check_spelling(filename, 
                 .report_error = .report_error,
                 pre_release = pre_release,
                 bib_files = bib_files)
  if (!pre_release && exists("authors_in_bib_and_doc") && not_length0(authors_in_bib_and_doc)){
    notes <- notes + 1L
    
    authors_in_bib_and_doc <- 
      authors_in_bib_and_doc[seq.int(1L, min(length(authors_in_bib_and_doc), 5L))]
    
    cat("NOTE: Skipped spell check for authors in bibliography.",
        "Please use \\citeauthor{} (preferred) or include the line\n\n\t% add_to_dictionary:", paste0(authors_in_bib_and_doc, collapse = " "), 
        "\n\nin your .tex file if the names have been spelled correctly. (Author names will NOT be skipped at pre-release.)\n")
  }
  cat(green(symbol$tick, "Spellcheck complete.\n"))

  check_labels(filename)

  cat(green(symbol$tick, "Labels checked.\n"))

  all_figs_tbls_refd <- figs_tbls_not_refd <- NULL
  check_all_figs_tbls_refd(filename, compile = compile, pre_release = pre_release)

  if (pre_release){
    cat(green(symbol$tick, "All figures and tables have a Xref.\n"))
  } else {
    if (is.null(all_figs_tbls_refd)){
      stop("Emergency stop: This is a bug. Please report.")
    }
    
    if (!all_figs_tbls_refd){
      notes <- notes + 1L
      cat(if (compile) "WARNING:" else  "NOTE:", 
          "Not all figures and tables referenced. ", 
          figs_tbls_not_refd)
    }
  }

  cat("\n")
  
  if (compile){
    full_dir_of_path <- getwd()
    md5_filename <- paste0(substr(tools::md5sum(filename), 0, 10),
                           substr(tools::md5sum(bib_file), 0, 10))
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
    if (file.exists(gsub("\\.tex", ".pdf", filename))){
      file.remove(gsub("\\.tex", ".pdf", filename))
    }
    
    cat("   Invoking pdflatex... ")
    options(warn = 2)
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    cat("complete.\n")
    cat("   Invoking biber...\n")
    system2(command = "biber",
            args = c("--onlylog", "-V", gsub("\\.tex$", "", filename)),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    check_biber()
    cat(green(symbol$tick, "biber validated citations.\n"))
    
    cat("   Rerunning pdflatex. Starting pass number 1")
    system2(command = "pdflatex",
            args = c("-draftmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    cat(" 2 ")
    system2(command = "pdflatex",
            args = c("-interaction=batchmode", filename),
            stdout = gsub("\\.tex$", ".log2", filename))
    
    log_result <- check_log(check_for_rerun_only = TRUE)
    reruns_required <- 2
    while (pre_release && !is.null(log_result) && log_result == "Rerun LaTeX."){
      cat(reruns_required + 1, " ", sep = "")
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
    
    if (pre_release){
      CenturyFootnote_suspect <- NULL
      check_CenturyFootnote()
      if (!CenturyFootnote_suspect){
        cat(green(symbol$tick, "\\CenturyFootnote correctly placed.\n"))
      } else {
        notes <- notes + 1
      }
      
      if (release){
        cat("Now preparing a release...\n")
        if (!dir.exists("RELEASE")){
          dir.create("RELEASE")
        }
        
        new_filename <- 
          read_lines(filename) %>%
          grep("^\\\\title\\{", ., perl = TRUE, value = TRUE) %>% 
          gsub("^\\\\title\\{(.+)\\}$", "\\1", ., perl = TRUE) %>%
          gsub("[^A-Za-z]", "-", ., perl = TRUE) %>%
          paste0(".pdf")
        
        # Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
        if (embed){
          embedFonts(gsub("\\.tex$", ".pdf", filename),
                     outfile = file.path(full_dir_of_path, "RELEASE", new_filename))
          cat(green(symbol$tick, "Fonts embedded.\n"))
        } else {
          file.copy(gsub("\\.tex$", ".pdf", filename), 
                    file.path(full_dir_of_path, "RELEASE", new_filename))
          cat("NOTE: Fonts not embedded, as requested.\n")
          notes <- notes + 1
        }
        
      } else {
        file.copy(paste0(report_name, ".pdf"), 
                  file.path(full_dir_of_path,
                            "PRE-RELEASE",
                            paste0(report_name, ".pdf")))
      }
      
      setwd(full_dir_of_path)
      cat("\n")
    }
  }
  
  cat(bgGreen(symbol$tick, "Report checked.\n"))
  if (pre_release){
    if (release){
      cat("Releaseable pdf written to ", file.path(path, "RELEASE", new_filename))
      cat("\nDONE.")
      
      lines <- read_lines(filename)
      if (!any(grepl("FrontPage", lines))){
        cat("\n\nNOTE: Did you forget to add the FrontPage to \\documentclass{grattan}?")
      }
      if (any(grepl("XX", lines[!or(grepl("tabularx", lines, perl = TRUE),
                                    grepl("^%", lines, perl = TRUE))]))){
        cat("\nWARNING: Found XX in document.")
      }
    } else {
      cat("Pre-release version written to ", file.path(path, "PRE-RELEASE", gsub("\\.tex$", ".pdf", filename)))
      cat("\nDONE.")
    }
  }
  
  if (file.exists("./travis/grattanReport/error-log.tsv")){
    prev_build_status <-
      fread("./travis/grattanReport/error-log.tsv") %>%
      last %>%
      .[["build_status"]]
    append <- TRUE
  } else {
    prev_build_status <- "None"
    append <- FALSE
  }
  
  if (prev_build_status %in% c("None", "Broken", "Still failing")){
    build_status <- "Fixed"
    if (output_method == "gmailr"){
      message <- gmailr::mime(
        To = "hugh.parsonage@gmail.com", #email_addresses, 
        From = "hugh.parsonage@gmail.com",
        Subject = paste0("Fixed: ", report_name)
      ) %>%
        gmailr::html_body(body = paste0(c("grattanReporter returned no error.")))
      gmailr::send_message(message)
    }
  } else {
    build_status <- "OK"
  }
  
  if (!.no_log){
    data.table(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               build_status = build_status, 
               error_message = "NA") %>%
      fwrite("./travis/grattanReport/error-log.tsv",
             sep = "\t",
             append = append)
    if (notes > 0){
      if (notes > 1){
        cat("\n\tThere were", notes, "notes.")
      } else {
        cat("\n\tThere was 1 note.")
      }
    } 
  }
  
invisible(NULL)
}
