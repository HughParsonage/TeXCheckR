
library(magrittr)
library(data.table)
source("./data-raw/put-twocolumn_atop.R", chdir = TRUE)
# source("./data-raw/put-dictionaryAU.R", chdir = TRUE)
source("./data-raw/put-newspaper_by_url.R", chdir = TRUE)

ordinal_pattern <-
  paste0("((?<!1)1(\\\\textsuperscript\\{)?st)",
         "|",
         "((?<!1)2(\\\\textsuperscript\\{)?nd)",
         "|",
         "((?<!1)3(\\\\textsuperscript\\{)?rd)",
         "|",
         "(([04-9]|(1[1-3]))(\\\\textsuperscript\\{)?th)")
stopifnot(identical(grepl(ordinal_pattern,
                          c("3rd", "11th", "21st", "13th", "13rd", "101st", "11st", "funding", "3\\textsuperscript{rd}"),
                          perl = TRUE),
                    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)))

lc_govt_pattern <-
  paste0("(?:",
         paste0("(?:Federal)",
                "|",
                "(?:Commonwealth)",
                "|",
                "(?:[tT]he Australian)",
                "|",
                "(?:N(?:ew )?S(?:outh )?W(?:ales)?)",
                "|",
                "(?:Vic(?:torian?)?)",
                "|",
                "(?:Q(?:ueens)?l(?:an)?d)",
                "|",
                "(?:S(?:outh )?A(?:ustralian?)?)",
                "|",
                "(?:W(?:estern )?A(?:ustralian?)?)",
                "|",
                "(?:N(?:orthern? )?T(?:erritory)?)",
                "|",
                "(?:A(?:ustralian )?C(?:apital )?T(?:erritory)?)"),
         ") government(?!s)",
         "(?!\\s(?:schools?))")

punctuation <- c(".", ",", ":", ";", "'", '"', "?", "-", "!")

devtools::use_data(twocolumn_atop,
                   # dictionaryAU,
                   newspaper_by_url,
                   ordinal_pattern,
                   lc_govt_pattern,
                   punctuation,
                   internal = TRUE,
                   overwrite = TRUE)
