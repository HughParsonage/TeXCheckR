
wrongly_spelled_words_txt <- readLines("./data-raw/wrongly_spelled_words.txt", skipNul = TRUE)
wrongly_spelled_words_txt <- wrongly_spelled_words_txt[order(wrongly_spelled_words_txt)]
writeLines(wrongly_spelled_words_txt, "./data-raw/wrongly_spelled_words.txt")

wrongly_spelled_words <-
  paste0("(", paste0(
    wrongly_spelled_words_txt,
    collapse = ")|("),
    ")")

devtools::use_data(wrongly_spelled_words, overwrite = TRUE)
