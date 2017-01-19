
`%notin%` <- Negate(`%in%`)

AND <- `&&`
OR <- `||`

not_length0 <- function(x) as.logical(length(x))

# takes a vector of froms and tos and takes their union
seq.default.Vectorized <- function(x, y)
  Vectorize(seq.default, vectorize.args = c("from", "to"))(x, y)

Seq_union <- function(x, y){
  if (length(x) == 1L && length(y) == 1L){
    seq.int(x, y)
  } else {
    if (length(x) == length(y)){
      unlist(seq.default.Vectorized(x, y))
    } else {
      lengthx <- length(x)
      lengthy <- length(y)
      if (lengthx != 1L && lengthy != 1L){
        stop("x and y must have the same length if neither have length 1.")
      }
      if (lengthx == 1L){
        Seq_union(rep(x, lengthy), y) %>%
          unique.default
      } else {
        Seq_union(x, rep(y, lengthx)) %>%
          unique.default
      }
    }
  }
}

rev_forename_surname_bibtex <- function(author_fields){
  full_names <-
    lapply(author_fields, strsplit, " and ") %>%
    lapply(unlist, recursive = FALSE)

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

  lapply(out, FUN = function(author_name){
    lapply(author_name, paste0, collapse = ", ")
  }) %>%
    sapply(FUN = function(authors){
      paste0(authors, collapse = " and ")
    })
}

nth_max <- function(x, n){
  n <- length(x)
  sort(x, partial = n - 1)[n - 1]
}

nth_min <- function(x, n){
  sort(x)[n]
}

nth_min.int <- function(x, n){
  sort.int(x)[n]
}
