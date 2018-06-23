.onLoad <- function(libname = find.package("TeXCheckR"), pkgname = "TeXCheckR"){

  op <- options()
  opTeXCheckR <- list(
    "TeXCheckR.capture.output" = FALSE,
    "TeXCheckR.messages" = !identical(Sys.getenv("TEXCHECKR.MESSAGES"), "FALSE"),
    "TeXCheckR.halt_on_error" = FALSE
  )
  toset <- !(names(opTeXCheckR) %in% names(op))
  if (any(toset)) options(opTeXCheckR[toset])
  
  
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(c(".",
                             # data.table
                             "N", "GRP", "I",
                             "correctly_spelled_words",
                             "wrongly_spelled_words", 
                             "valid_English_contractions",
                             "CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE",
                             "not_all_figs_tbls_refd",
                             "not_all_figs_tbls_refd.lab"))

}
