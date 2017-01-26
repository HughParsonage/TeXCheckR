.onLoad <- function(libname = find.package("grattanReporter"), pkgname = "grattanReporter"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".", "correctly_spelled_words", "wrongly_spelled_words", "valid_English_contractions", "CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE", "not_all_figs_tbls_refd", "not_all_figs_tbls_refd.lab"))

}
