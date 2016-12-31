
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

rev_forename_surname <- function(noms){
  # John Daley --> Daley, John
  gsub(paste0("([A-Z](?:[a-z]*|\\.?))",
              " ",
              # van etc
              # \2
              "((?:v[ao]n)|(?:der?)|(?:di))*",
              # \3
              "( )?",
              # \4
              "([A-Z][a-z]+)"
              ),
       "\\4, \\1\\3\\2",
       noms,
       perl = TRUE)
}

rev_forename_surname_bibtex <- function(author_fields){
  author_fields %>%
  strsplit(split = " and ", fixed = TRUE)  %>%
  lapply(rev_forename_surname) %>%
    lapply(paste0, collapse = " and ") %>%
    unlist
}

