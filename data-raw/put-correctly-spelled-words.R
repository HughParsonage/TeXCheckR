
correctly_spelled_words_txt <- readLines("./data-raw/correctly_spelled_words.txt", skipNul = TRUE)
correctly_spelled_words_txt <- correctly_spelled_words_txt[order(correctly_spelled_words_txt)]
writeLines(correctly_spelled_words_txt, "./data-raw/correctly_spelled_words.txt")

correctly_spelled_words <- correctly_spelled_words_txt

devtools::use_data(correctly_spelled_words, overwrite = TRUE)
