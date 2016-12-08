correctly_spelled_words <-
  paste0("(", paste0(
         readLines("./data-raw/correctly_spelled_words.txt"),
         collapse = ")|("),
         ")")

devtools::use_data(correctly_spelled_words, overwrite = TRUE)
