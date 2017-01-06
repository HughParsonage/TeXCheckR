
source("./data-raw/put-twocolumn_atop.R", chdir = TRUE)
source("./data-raw/put-dictionaryAU.R", chdir = TRUE)
source("./data-raw/put-newspaper_by_url.R", chdir = TRUE)

devtools::use_data(twocolumn_atop,
                   dictionaryAU,
                   newspaper_by_url,

                   internal = TRUE,
                   overwrite = TRUE)
