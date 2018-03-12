
AND <- `&&`
OR <- `||`

not_length0 <- function(x) as.logical(length(x))

`%notchin%` <- function(lhs, rhs) {
  !{lhs %chin% rhs}
}

lead <- function(x, n = 1L, default = NA) shift(x, type = "lead", n = n, fill = default)
 lag <- function(x, n = 1L, default = NA) shift(x, type = "lag", n = n, fill = default)

fill_blanks <- function(S) {
   # from zoo
   L <- !is.na(S)
   c(S[L][1L], S[L], use.names = FALSE)[cumsum(L) + 1L]
}
 
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

move_to <- function(to.dir, from.dir = ".", pattern = "\\.((pdf)|(tex)|(cls)|(sty)|(Rnw)|(bib)|(png)|(jpg))$"){
  x <- list.files(path = from.dir,
                  pattern = pattern,
                  full.names = TRUE,
                  recursive = TRUE,
                  include.dirs = FALSE)
  x.dirs <- file.path(to.dir, 
                      list.dirs(path = from.dir, recursive = TRUE, full.names = TRUE))
  lapply(x.dirs, hutils::provide.dir)
  file.copy(x, file.path(to.dir, x), overwrite = TRUE, recursive = FALSE)
  setwd(to.dir)
  cat("   Attempting compilation in temp directory:", to.dir, "\n")
}

r2 <- function(a, b) sprintf("%s%s", a, b)
r3 <- function(a, b, d) sprintf("%s%s%s", a, b, d)
r4 <- function(a, b, d, e) sprintf("%s%s%s%s", a, b, d, e)
r5 <- function(a, b, d, e, f) sprintf("%s%s%s%s%s", a, b, d, e, f)
r9 <- function(a1, a2, a3, a4, a5, a6, a7, a8, a9) sprintf("%s%s%s%s%s%s%s%s%s", a1, a2, a3, a4, a5, a6, a7, a8, a9)

trimws_if_char <- function(x) if (is.character(x)) stri_trim_both(x) else x

# for printing parsed lines
print_transpose_data.table <- function(DT, file = "") {
  cat <- function(...) base::cat(..., file = file, append = TRUE)
  
  max_nchar <- function(v) {
    v[v == "\\"] <- "@"
    v_na <- is.na(v)
    out <- as.character(v)
    out[v_na] <- ""
    max(nchar(encodeString(out), type = "width"))
  }
  
  char_width <- max(vapply(DT, max_nchar, integer(1)))
  max_width_names <- max(nchar(names(DT)))
  
  for (var in names(DT)) {
    cat(formatC(var, width = max_width_names), ":")
    v <- DT[[var]]
    v_na <- is.na(v)
    v <- as.character(v)
    v[v == "\\"] <- "@"
    v[v_na] <- "."
    v <- formatC(v, width = char_width)
    # stop(var)
    cat(v, sep = "")
    cat("\n")
  }
  
}

return_first_nonNA <- function(x) {
  if (anyNA(x)) {
    not_nax <- !is.na(x)
    if (any(not_nax)) {
      out <- first(x[not_nax])
    } else {
      out <- x[1]
    }
  } else {
    out <- x[1]
  }
  out
}

# testthat
is_testing <- function() {
  requireNamespace("testthat", quietly = TRUE) &&
    utils::packageVersion("testthat") >= package_version("2.0.0") &&
    testthat::is_testing()
}






