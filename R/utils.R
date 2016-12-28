
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
