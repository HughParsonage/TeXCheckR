
library(magrittr)
library(data.table)
source("./data-raw/put-twocolumn_atop.R", chdir = TRUE)
# source("./data-raw/put-dictionaryAU.R", chdir = TRUE)
source("./data-raw/put-newspaper_by_url.R", chdir = TRUE)
source("./data-raw/put-Grattan-staff.R", chdir = FALSE)

devtools::use_data(twocolumn_atop,
                   # dictionaryAU,
                   newspaper_by_url,
                   Grattan_staff,

                   internal = TRUE,
                   overwrite = TRUE)
