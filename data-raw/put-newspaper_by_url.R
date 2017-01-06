newspaper_by_url <-
  data.table::fread("newspaper-by-url.tsv") %>%
  setkey(journal_from_url)
