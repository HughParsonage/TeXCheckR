
`%notin%` <- Negate(`%in%`)

not_length0 <- function(x) as.logical(length(x))

# takes a vector of froms and tos and takes their union
Seq_union <- function(x, y){
  if (length(x) == 1L && length(y) == 1L){
    seq.int(x, y)
  } else {
    lengthx <- length(x)
    lengthy <- length(y)
    if (lengthx == lengthy){
      unlist(Vectorize(seq.default, vectorize.args = c("from", "to"))(x, y))
    } else {
      if (length(x) != 1L && lengthy != 1L){
        stop("x and y must have the same length if neither have length 1.")
      }
      if (lengthx == 1L){
        Seq_union(rep(x, lengthy), y) %>%
          unique
      } else {
        Seq_union(x, rep(y, lengthx)) %>%
          unique
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

print_error_context <- function(line_no = NULL,
                                context = NULL,
                                ..., 
                                error.symbol = bgRed(symbol$cross), 
                                console = c("travis", "twitter")){
  console <- match.arg(console)
  # Printing requirements:
  ## 1. Cross
  ## 2. Line no (if applicable)
  ## 3. Context
  ## 4. Suggeston.
  switch(console,
         "travis" = {
           cat(error.symbol, " ", line_no, ": ", context, ..., sep = "")
         }, 
         "twitter" = {
           warning("Not implemented.")
           if (FALSE){
             # twitteR::updateStatus()
           }
         })
}
