#' Check consecutive typeset words
#' @param path Path containing the LaTeX file.
#' @param latex_file The LaTeX file (without path) whose output will be checked.
#' @param md5sum.ok The output of \code{md5sum} of an acceptable LaTeX file. Since some repeated words will be spurious,
#' you can use the \code{md5sum} of the output of this function.
#' @param outfile A file to which the output can be saved. If \code{NULL}, the default, the output is printed to the console (and not saved).
#' @param outfile.append (logical, default: \code{FALSE}). Append or overwrite \code{outfile} if specified? If \code{FALSE}, the default, and file exists, \code{outfile} will be overwritten.
#' @return \code{NULL} if the \code{LaTeX} document does not create a PDF with lines repeated. 
#' An error if words are repeated on consecutive lines, together with \code{cat()} output of the offending lines. The output is presented in 'stanzas': 
#' \preformatted{'<Repeated word>'
#'         <Context>
#' }
#' 
#' for example a document that results in the following lines, notably the repetition of \emph{household}, the output would be:
#' \preformatted{'household'
#'         affordable. This `mortgage burden' is often defined as the proportion of
#'         household income spent on repaying a mortgage. Depending on the
#'         household income measure used, the mortgage burden on a newly
#'         purchased first home, assuming a person borrows 80 per cent of the
#'         value of the home, is currently lower than much of the period between
#' } 
#' 
#' Lastly the error message contains the \code{\link[tools]{md5sum}} of the file is returned in the error message, so it can be supplied to \code{md5sum.ok}. 
#' @export


check_consecutive_words <- function(path = ".",
                                    latex_file = NULL,
                                    md5sum.ok = NULL,
                                    outfile = NULL,
                                    outfile.append = FALSE) {
  
  if (!nzchar(Sys.which("pdftotext"))) {
    stop("'pdftotext' not found on system path, but is required for check_consecutive_words().")
  }

  pdf.files <- list.files(path = path, pattern = "\\.pdf$")
  if (length(pdf.files) != 1L) {
    stop("`path` did not contain a single PDF file.")
  }

  if (!is.null(outfile)) {
    file.create(outfile)
    .outfile <- normalizePath(outfile, winslash = "/")
  }
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(path)

  stopifnot(file.exists(latex_file))

  md5sum_latex_file <- tools::md5sum(latex_file)

  if (!is.null(md5sum.ok) && md5sum_latex_file == md5sum.ok){
    return(NULL)
  }
  if (!dir.exists(file.path(tempdir(), "consecutive-words"))){
    dir.create(file.path(tempdir(), "consecutive-words"))
  }
  
  time <- format(Sys.time(), "%Y-%m-%d-%H%M")
  if (!dir.exists(file.path(tempdir(), "consecutive-words", time))){
    dir.create(file.path(tempdir(), "consecutive-words", time))
  }
  move_to(file.path(tempdir(), "consecutive-words", time), pattern = NULL)
  
  # Avoid spurious marks on 'Grattan Institute' or the name of the report
  if (file.exists("grattan.cls")) {
    grattan.cls <- readLines("grattan.cls")
    # Can't simply omit the headers -- they will affect the makeup of the rest of the 
    # text.  Best bet is to phantom them.
    grattan.cls %>%
      gsub("\\normalfont Grattan Institute \\@YEAR", "\\normalfont", ., fixed = TRUE) %>%
      gsub("\\textcolor{theGrey}{\\thepage}", "\\phantom{\\thepage}", ., fixed = TRUE) %>%
      gsub("\\textcolor{theGrey}{\\mytitle}", "\\phantom{\\mytitle}", ., fixed = TRUE) %>%
      writeLines("grattan.cls")
  }

  if (file.exists("CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")) {
    stop('"CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex" exists in the path. This was unexpected.')
  }


  # Copy the original file to a temporary, but work on the original
  # This obviates the need to run biber, makeglossaries etc: we just
  # use the extant auxiliary files.
  file.copy(latex_file, paste0("CHECK-CONSECUTIVE-WORDS-", latex_file), overwrite = FALSE)


  read_lines(latex_file) %>%
    gsub("\\begin{document}",
         "\\input{CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP}\n\\begin{document}",
         x = ., 
         fixed = TRUE) %>%
    # Safe to omit the bibliography for now
    gsub("\\printbibliography", "", x = ., fixed = TRUE) %>%
    write_lines(latex_file)

  # Put the text from 
  # http://tex.stackexchange.com/questions/341842/convert-twocolumn-layout-to-onecolumn-with-identical-linebreaks

  writeLines(twocolumn_atop, con = "CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")

  # Run pdflatex (biber should have already been run).
  system(paste("pdflatex -draftmode", latex_file),
         show.output.on.console = FALSE,
         ignore.stderr = TRUE)
  system(paste("pdflatex -draftmode", latex_file),
         show.output.on.console = FALSE, 
         ignore.stderr = TRUE)
  system(paste("pdflatex -interaction=batchmode", latex_file))

  # Replace the latex files
  file.copy(paste0("CHECK-CONSECUTIVE-WORDS-", latex_file), latex_file, overwrite = TRUE)
  file.remove("CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")

  system(paste("pdftotext -layout", gsub("\\.tex$", ".pdf", latex_file)))

  typeset_lines <- read_lines(gsub("\\.tex$", ".txt", latex_file))

  # Only consider words between overview and bibliography
  if (OR("Overview" %chin% typeset_lines,
         "\fOverview" %chin% typeset_lines)) {
    overview.line <- which(typeset_lines %chin% c("Overview", "\fOverview"))
    typeset_lines <- typeset_lines[-seq.int(1L, overview.line)]
  }

  if (OR("Bibliography" %chin% typeset_lines,
         "\fBibliography" %chin% typeset_lines)) {
    bibliography.line <- which(typeset_lines %chin% c("Bibliography", "\fBibliography"))
    typeset_lines <- typeset_lines[seq_len(max(bibliography.line) - 1L)]
  }

  file.remove(gsub("\\.tex$", ".txt", latex_file))
  #

  valid_typeset_lines <- typeset_lines[grepl("^(\\w+)\\b.*$", typeset_lines, perl = TRUE)]

  first_words <- gsub("^(\\w+)\\b.*$", "\\1", valid_typeset_lines, perl = TRUE)

  is_repeated <-
    first_words == shift(first_words,
                         type = "lag",
                         n = 1L,
                         fill = "Unlikely to be repeated") &
    nzchar(first_words) &
    # Not only numbers
    !grepl("^[0-9]+$", first_words, perl = TRUE)

  repeated_words <- first_words[is_repeated]

  if (length(repeated_words) > 0) {
    if (!is.null(outfile)) {
      cat <- function(...) base::cat(..., file = .outfile, append = TRUE)
    }
    
    cat("'<Repeated word>'\n\t<Context>\n")
    for (repetition in seq_along(repeated_words)){
      cat("'", repeated_words[repetition], "'\n\t", sep = "")
      cat(valid_typeset_lines[rep(which(is_repeated)[repetition], each = 5) + -2:2], sep = "\n\t")
      cat("\n\n")
    }

    # out <- data.table(repeated_words = repeated_words)

    stop("Repeated words.", "\n", "\n",
         "If document acceptable, md5sum.ok = '", md5sum_latex_file, "'")
  }




}
