
correctly_spelled_words_txt <- 
  c(readLines("./data-raw/correctly_spelled_words.txt", skipNul = TRUE), 
    readLines("./data-raw/proper-nouns-1.txt"))
correctly_spelled_words_txt <- correctly_spelled_words_txt[order(correctly_spelled_words_txt)]
writeLines(correctly_spelled_words_txt, "./data-raw/correctly_spelled_words.txt")

CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE <-
  grep("[A-Z]", correctly_spelled_words_txt, value = TRUE)

correctly_spelled_words <- setdiff(correctly_spelled_words_txt, CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE)

devtools::use_data(correctly_spelled_words,
                   CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE,
                   overwrite = TRUE)
