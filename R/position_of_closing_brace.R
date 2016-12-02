

position_of_closing_brace <- function(line, prefix){
  lines_split <- strsplit(gsub(paste0("^.*", prefix), "", line, perl = TRUE), split = "")[[1]]
  min(which(cumsum(lines_split == "{") - cumsum(lines_split == "}") == 0))
}
