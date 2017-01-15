
Grattan_staff <- 
  readLines("./data-raw/Grattan_staff.txt") %>%
  .[nchar(.) > 0]
