
source("./data-raw/put-twocolumn_atop.R", chdir = TRUE)
source("./data-raw/put-dictionaryAU.R", chdir = TRUE)

devtools::use_data(twocolumn_atop,
                   dictionaryAU,

                   internal = TRUE,
                   overwrite = TRUE)
