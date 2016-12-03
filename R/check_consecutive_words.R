#' Check consecutive typeset words
#' @param path Path containing the latex file.
#' @param latex_file The LaTeX file whose output will be checked.
#' @param md5sum.ok The output of \code{md5sum} of an acceptable LaTeX file. Since some repeated words will be spurious,
#' you can use the md5sum of the output of this function.
#' @return An error if words are repeated on consecutive lines, together with cat() output of the offending lines.
#' Lastly the \code{tools::md5sum} of the file is returned in the error message, so it can be supplied to \code{md5sum.ok}.
#' @export


check_consecutive_words <- function(path = ".", latex_file = NULL, md5sum.ok = NULL){
  pdf.files <- list.files(path = path, pattern = "\\.pdf$")

  stopifnot(length(pdf.files) == 1L)

  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(path)

  md5sum_latex_file <- tools::md5sum(latex_file)

  if (!is.null(md5sum.ok) && md5sum_latex_file == md5sum.ok){
    return(NULL)
  }

  if (file.exists("CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex")){
    stop('"CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP.tex" exists in the path. This was unexpected.')
  }

  # Copy the original file to a temporary, but work on the original
  # This obviates the need to run biber, makeglossaries etc: we just
  # use the extant auxiliary files.
  file.copy(latex_file, paste0("CHECK-CONSECUTIVE-WORDS-", latex_file), overwrite = FALSE)

  readLines(latex_file) %>%
    gsub("\\begin{document}", "\\input{CHECK-CONSECUTIVE-WORDS-TWOCOLUMN-ATOP}\n\\begin{document}", x = ., fixed = TRUE) %>%
    writeLines(latex_file)

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
    typeset_lines <- typeset_lines[-seq.int(1, overview.line)]
  }

  if ("Bibliography" %in% typeset_lines){
    bibliography.line <- which(typeset_lines == "Bibliography")
    typeset_lines <- typeset_lines[1:(bibliography.line - 1)]
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

  if (length(repeated_words) > 0){
    for (repetition in seq_along(repeated_words)){
      cat(repeated_words[repetition], "\n", sep = "\n")
      cat(valid_typeset_lines[rep(which(is_repeated)[repetition], each = 5) + -2:2], sep = "\n")
      cat("\n\n")
    }

    # out <- data.table(repeated_words = repeated_words)

    stop("Repeated words.", "\n", "\n",
         "If document acceptable, md5sum.ok = '", md5sum_latex_file, "'")
  }




}
