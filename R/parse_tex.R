#' Parse LaTeX lines
#' @param tex_lines Character vector (as read from a \code{.tex} file).
#' @return A \code{data.table} where each row identifies a unique character in \code{tex_lines}.
#' \describe{
#' \item{\code{line_no}}{Matches the index of \code{tex_lines}.}
#' \item{\code{char_no}}{The character within \code{line_no}.}
#' \item{\code{char}}{The character. A single character.}
#' \item{\code{tex_group}}{The TeX group by default. Any delimiters can be used.}
#' \item{\code{optional_tex_group}}{(If any present), the optional TeX group.}
#' \item{\code{tgi}}{The number of braces opened at the \code{i}-th current TeX group level.}
#' \item{\code{GROUP_IDi}}{An integer identifying the unique contiguous block at the TeX group at or above the current group level.}
#' \item{\code{GROUP_IDi}}{The analog for optional groups.}
#' }
#' If \code{tex_lines} is zero-length, a null \code{data.table}.
#' 
#' @examples
#' parse_tex(c("A{}", "B[a]{b{c}{d}}z"))
#' # The version transposed:
#' #
#' #>          char : A{}B[a]{b{c}{d}}z
#' #>           tg1 : 011111122......22
#' #>           tg2 : 00000000011122222
#' #>           og1 : 00001111111111111
#' #>     GROUP_ID1 : .11....222222222.
#' #>     GROUP_ID2 : .........111222..
#' #> OPT_GROUP_ID1 : ....111..........
#' @export

parse_tex <- function(tex_lines) {
  if (!length(tex_lines)) {
    return(data.table())
  }
  tex_lines <- strip_comments(tex_lines, retain.percent.symbol = FALSE)
  nchar_tex_lines <- nchar(tex_lines)
  
  if (!any(nchar_tex_lines)) {
    return(data.table(char_no = seq_along(tex_lines),
                      line_no = seq_along(tex_lines),
                      column = 1L,
                      char = "",
                      openers = FALSE,
                      closers = FALSE,
                      opener_optional = FALSE,
                      closer_optional = FALSE,
                      tex_group = 0L,
                      optional_tex_group = 0L))
  }
  
  trailing_newlines <- max(which(tex_lines != ""))
  Tex_line_split_unlist <- unlist(strsplit(tex_lines, split = "", fixed = TRUE),
                                  use.names = FALSE, recursive = FALSE)
  n_char <- sum(nchar_tex_lines)
  
  opener <- Tex_line_split_unlist == "{"
  closer <- Tex_line_split_unlist == "}"
  opener_optional <- Tex_line_split_unlist == "["
  closer_optional <- Tex_line_split_unlist == "]"
  tex_group <- cumsum(opener) - cumsum(closer) + closer
  optional_tex_group <- cumsum(opener_optional) - cumsum(closer_optional) + closer_optional
  openers <- NULL
  
  out <- list(char_no = seq_len(n_char),
              line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
              column = unlist(lapply(nchar_tex_lines, seq_len), use.names = FALSE),
              char = Tex_line_split_unlist,
              openers = opener,
              closers = closer,
              opener_optional = opener_optional,
              closer_optional = closer_optional,
              tex_group = tex_group,
              optional_tex_group = optional_tex_group)
  setattr(out, "class", c("data.table", "data.frame"))
  
 
  # Nested tex group -- likely to be small
  # 0L to ensure blank documents don't cause obscure errors
  max_tex_group <- max(tex_group, 0L)
  alloc.col(out, n = 10L * max_tex_group + 10L)
  
  seq_max_tex_group <- seq_len(max_tex_group)
  
  tg <- sprintf("tg%s", seq_max_tex_group)
  GROUP_IDz <- sprintf("GROUP_ID%s", seq_max_tex_group)
  
  # Identify tex groups
  # A [b] \\cde[fg][hi]{jk} \\mn[o[p]]{q}.
  # 0000000000000000000111100000000000222
  
  setindexv(out, "tex_group")
  for (j in seq_max_tex_group) {
    tgj <- tg[j]

    out[tex_group <= j, (tgj) := cumsum(openers & tex_group == j)]
    
    which_tex_group_geq_j <- which(tex_group >= j)
    
    GROUP_IDj <- GROUP_IDz[j]
    out[tex_group == j, (GROUP_IDj) := .GRP, by = c("optional_tex_group", tgj)]
    
    out[tex_group >= j, (GROUP_IDj) := fill_blanks(.subset2(out, GROUP_IDj)[which_tex_group_geq_j])]
  }
  
  # Uniquely identify optional groups
  # A [b] \\cde[fg][hi]{jk} \\mn[o[p]]{q}.
  # 0011100000022223333000000000445554000
  
  max_opt_group <- max(optional_tex_group, 0L)
  seq_max_opt_group <- seq_len(max_opt_group)
  og <- sprintf("og%s", seq_max_opt_group)
  OPT_GROUP_IDz <- sprintf("OPT_GROUP_ID%s", seq_max_opt_group)
  
  setindexv(out, "optional_tex_group")
  for (k in seq_max_opt_group) {
    ogk <- og[k]
    
    out[optional_tex_group <= k, (ogk) := cumsum(opener_optional & optional_tex_group == k)]
    
    OPT_GROUP_IDj <- OPT_GROUP_IDz[k]
    out[optional_tex_group == k, (OPT_GROUP_IDj) := .GRP, by = c(ogk)]
    
    which_opt_group_geq_k <- which(optional_tex_group >= k)
    
    out[optional_tex_group >= k,
        (OPT_GROUP_IDj) := fill_blanks(.subset2(out, OPT_GROUP_IDj)[which_opt_group_geq_k])]
  }
  
  out
}


# # nocov
# parse_tex2 <- function(tex_lines) {
#   .NotYetImplemented()
#   Tex_line_split_unlist <-
#     unlist(strsplit(tex_lines,
#                     split = "",
#                     fixed = TRUE),
#            use.names = FALSE,
#            recursive = FALSE)
#   nchar_tex_lines <- nchar(tex_lines)
#   n_char <- sum(nchar(tex_lines))
# 
#   opener <- Tex_line_split_unlist == "{"
#   closer <- Tex_line_split_unlist == "}"
#   opener_optional <- Tex_line_split_unlist == "["
#   closer_optional <- Tex_line_split_unlist == "]"
#   tex_group <- cumsum(opener) - cumsum(closer) + closer
#   optional_tex_group <- cumsum(opener_optional) - cumsum(closer_optional) + closer_optional
#   openers <- NULL
# 
#   out <- list(char_no = seq_len(n_char),
#               line_no = rep(seq_along(tex_lines), times = nchar_tex_lines),
#               column = unlist(lapply(nchar_tex_lines, seq_len), use.names = FALSE),
#               char = Tex_line_split_unlist,
#               openers = opener,
#               closers = closer,
#               opener_optional = opener_optional,
#               closer_optional = closer_optional,
#               tex_group = tex_group,
#               optional_tex_group = optional_tex_group)
# 
#   # Nested tex group -- likely to be small
#   # 0L to ensure blank documents don't cause obscure errors
#   max_tex_group <- max(tex_group, 0L)
# 
#   seq_max_tex_group <- seq_len(max_tex_group)
# 
#   tg <- sprintf("tg%s", seq_max_tex_group)
#   GROUP_IDz <- sprintf("GROUP_ID%s", seq_max_tex_group)
#   vacant_entry <- rep_len(NA_integer_, n_char)
# 
#   for (j in seq_max_tex_group) {
#     tgj <- tg[j]
# 
#     out[[tgj]] <- vacant_entry
#     tg_leq_j <- .subset2(out, "tex_group") <= j
#     out[[tgj]][tg_leq_j] <- cumsum(openers[tg_leq_j] & tex_group[tg_leq_j] == j)
# 
#     GROUP_IDj <- GROUP_IDz[j]
#     out[[GROUP_IDj]] <- vacant_entry
#     out[[GROUP_IDj]] <- fmatch()
#     out[tex_group == j, (GROUP_IDj) := .GRP, by = c("optional_tex_group", tgj)]
# 
#     which_tex_group_geq_j <- which(tex_group >= j)
#     out[(which_tex_group_geq_j), (GROUP_IDj) := fill_blanks(.subset2(out, GROUP_IDj)[which_tex_group_geq_j])]
#   }
# 
#   # Uniquely identify optional groups
#   # A [b] \\cde[fg][hi]{jk} \\mn[o[p]]{q}.
#   # 0011100000022223333000000000445554000
# 
#   max_opt_group <- max(optional_tex_group, 0L)
#   seq_max_opt_group <- seq_len(max_opt_group)
#   og <- sprintf("og%s", seq_max_opt_group)
#   OPT_GROUP_IDz <- sprintf("OPT_GROUP_ID%s", seq_max_opt_group)
# 
#   for (k in seq_max_opt_group) {
#     ogk <- og[k]
# 
#     out[optional_tex_group <= k, (ogk) := cumsum(opener_optional & optional_tex_group == k)]
# 
#     OPT_GROUP_IDj <- OPT_GROUP_IDz[k]
#     out[optional_tex_group == k, (OPT_GROUP_IDj) := .GRP, by = c(ogk)]
# 
#     which_opt_group_geq_k <- which(optional_tex_group >= k)
#     out[(which_opt_group_geq_k), (OPT_GROUP_IDj) := fill_blanks(.subset2(out, OPT_GROUP_IDj)[which_opt_group_geq_k])]
#   }
# 
#   out
# }






