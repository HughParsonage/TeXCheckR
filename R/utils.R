
`%notin%` <- Negate(`%in%`)

not_length0 <- function(x) length(x) > 0

# takes a vector of froms and tos and takes their union
Seq_union <- function(x, y){
  if (length(x) == 1L && length(y) == 1L){
    seq.int(x, y)
  } else {
    if (length(x) != 1L && length(y) != 1L){
      c(Vectorize(seq.default, vectorize.args = c("from", "to"))(x, y))
    } else {
      unlist(Vectorize(seq.default, vectorize.args = c("from", "to"))(x, y))
    }
  }
}

rev_forename_surname_bibtex <- function(author_fields){
  full_names <- sapply(author_fields, strsplit, " and ", USE.NAMES = FALSE)

  comma_name <-
    full_names %>%
    lapply(grepl, pattern = ", ", fixed = TRUE)

  forename_surnames <-
    full_names %>%
    lapply(strsplit, split = "(, )|(\\s((?!(?:v[ao]n)|(?:der?)|(?:di))(?=(\\w+$))))", perl = TRUE)

  out <- forename_surnames

  for (field in seq_along(author_fields)){
    for (nom in seq_along(full_names[[field]])){
      if (!comma_name[[field]][[nom]]){
        out[[field]][[nom]] <- rev(out[[field]][[nom]])
      }
    }
  }
  unlist(out, recursive = FALSE) %>%
    sapply(paste0, collapse = ", ") %>%
    paste0(collapse = " and ")
}

