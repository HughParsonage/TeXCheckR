
twocolumn_atop <- readLines("twocolumn-atop.tex")

devtools::use_data(twocolumn_atop, internal = TRUE)
