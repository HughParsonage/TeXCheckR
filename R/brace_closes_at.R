#' Brace closes at
#' @description Where do braces close?
#' @param tex_line A single line.
#' @param position_of_opening_brace An integer giving the position of the opening brace in question.
#' @return The positions of the closing brace matching the opening braces at \code{position_of_opening_brace}.

braces_closes_at <- function(tex_line, position_of_opening_brace){
  sapply(position_of_opening_brace, function(x){
    tex_line_split <- strsplit(tex_line, split = "", fixed = TRUE)[[1]]
    lines_split <-
      tex_line_split[-c(1:(x - 1))]

    tex_group <- cumsum(lines_split == "{") - cumsum(lines_split == "}")
    min(which(tex_group < tex_group[1])) - 1L
  })
}
