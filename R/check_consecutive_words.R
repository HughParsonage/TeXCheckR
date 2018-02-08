#' Check consecutive typeset words
#' @param path Path containing the LaTeX file.
#' @param latex_file The LaTeX file (without path) whose output will be checked.
#' @param md5sum.ok The output of \code{md5sum} of an acceptable LaTeX file. Since some repeated words will be spurious,
#' you can use the \code{md5sum} of the output of this function.
#' @return An error if words are repeated on consecutive lines, together with cat() output of the offending lines.
#' Lastly the \code{tools::md5sum} of the file is returned in the error message, so it can be supplied to \code{md5sum.ok}. \code{NULL} otherwise.
#' @export


check_consecutive_words <- function(path = ".", latex_file = NULL, md5sum.ok = NULL){
  
  if (!nzchar(Sys.which("pdftotext"))) {
    stop("'pdftotext' not found on system path, but is required for check_consecutive_words().")
  }

  pdf.files <- list.files(path = path, pattern = "\\.pdf$")
  if (length(pdf.files) != 1L) {
    stop("`path` did not contain a single PDF file.")
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
    grattan.cls %>%
      gsub("\\normalfont Grattan Institute \\@YEAR", "\\normalfont", ., fixed = TRUE) %>%
      gsub("\\thepage", "\\phantom{\\thepage}", ., fixed = TRUE) %>%
      gsub("\\mytitle", "\\phantom{\\mytitle}", ., fixed = TRUE) %>%
      writeLines("grattan.cls")
  }

  if (file.exists("CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")){
    stop('"CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex" exists in the path. This was unexpected.')
  }


  # Copy the original file to a temporary, but work on the original
  # This obviates the need to run biber, makeglossaries etc: we just
  # use the extant auxiliary files.
  file.copy(latex_file, paste0("CHECK-CONSECUTIVE-WORDS-", latex_file), overwrite = FALSE)

  readLines(latex_file) %>%
    gsub("\\begin{document}", "\\input{CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP}\n\\begin{document}",
         x = ., 
         fixed = TRUE) %>%
    # Safe to omit the bibliography for now
    gsub("\\printbibliography", "", x = ., fixed = TRUE) %>%
    writeLines(latex_file)

  warning(latex_file, "has been modified. Recompile ")

  # Put the text from http://tex.stackexchange.com/questions/341842/convert-twocolumn-layout-to-onecolumn-with-identical-linebreaks
  writeLines(twocolumn_atop, con = "CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")

  # Run pdflatex (biber should have already been run).
  system(paste("pdflatex -draftmode", latex_file), show.output.on.console = FALSE)
  system(paste("pdflatex -draftmode", latex_file), show.output.on.console = FALSE)
  system(paste("pdflatex -interaction=batchmode", latex_file))

  # Replace the latex files
  file.copy(paste0("CHECK-CONSECUTIVE-WORDS-", latex_file), latex_file, overwrite = TRUE)
  file.remove("CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")

  system(paste("pdftotext -layout", gsub("\\.tex$", ".pdf", latex_file)))

  typeset_lines <- readLines(gsub("\\.tex$", ".txt", latex_file), warn = FALSE)

  # Only consider words between overview and bibliography
  if ("Overview" %in% typeset_lines){
    overview.line <- which(typeset_lines == "Overview")
    typeset_lines <- typeset_lines[-seq.int(1L, overview.line)]
  }

  if ("Bibliography" %in% typeset_lines){
    bibliography.line <- which(typeset_lines == "Bibliography")
    typeset_lines <- typeset_lines[seq_len(bibliography.line - 1L)]
  }

  file.remove(gsub("\\.tex$", ".txt", latex_file))
  #

  valid_typeset_lines <- typeset_lines[grepl("^(\\w+)\\b.*$", typeset_lines, perl = TRUE)]

  first_words <- gsub("^(\\w+)\\b.*$", "\\1", valid_typeset_lines, perl = TRUE)

  is_repeated <-
    first_words == data.table::shift(first_words,
                                     type = "lag",
                                     n = 1L,
                                     fill = "Unlikely to be repeated") &
    nchar(first_words) > 0 &
    # Not only numbers
    !grepl("^[0-9]+$", first_words, perl = TRUE)

  repeated_words <- first_words[is_repeated]

  if (length(repeated_words) > 0) {
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
