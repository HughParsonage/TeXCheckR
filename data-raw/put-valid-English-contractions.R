valid_English_contractions <- readLines("valid_English_contractions.txt", encoding = "UTF-8")

devtools::use_data(valid_English_contractions, overwrite = TRUE)
