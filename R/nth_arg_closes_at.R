



nth_arg_closes_at <- function(tex_lines, command_name, n = 1L){
  Command_locations <- 
    stringi::stri_locate_all_fixed(str = tex_lines, 
                                   pattern = paste0("\\", command_name))
  
  Tex_line_split <- strsplit(tex_lines, split = "")
  
  lapply(seq_along(tex_lines), function(i){
    command_locations <- Command_locations[[i]][, 2]
    tex_line_split <- Tex_line_split[[i]]
    tex_group <- cumsum(tex_line_split == "{") - cumsum(tex_line_split == "}")
    tex_group_at_command_locations <- tex_group[command_locations]
    out <- integer(n)
    i <- 1
    for (tg in command_locations){
      out[[i]] <- 
        nth_min.int(which(and(tex_group == tex_group[tg],
                              and(seq_along(tex_group) > tg,
                                  tex_group == dplyr::lag(tex_group, 1L, default = 0) - 1))),
                    n = n)
      i <- i + 1
    }
    out
  })
}
