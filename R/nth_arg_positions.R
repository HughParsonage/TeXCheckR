


nth_arg_positions <- function(tex_lines, command_name, n = 1L){
  Command_locations <- 
    stringi::stri_locate_all_regex(str = tex_lines, 
                                   # If command = \a, must not also detect \ab
                                   pattern = paste0("\\\\", command_name, "(?![A-Za-z])"))
  
  Tex_line_split <- strsplit(tex_lines, split = "")
  
  lapply(seq_along(tex_lines), function(i){
    command_locations <- Command_locations[[i]][, 2]
    tex_line_split <- Tex_line_split[[i]]
    tex_group <- cumsum(tex_line_split == "{") - cumsum(tex_line_split == "}")
    tex_group_at_command_locations <- tex_group[command_locations]
    
    starts <- stops <- integer(length(command_locations))
    j <- 1
    for (tg in command_locations){
      
      starts[[j]] <- 
        1 + # below tells us the position of the opening *brace*
        nth_min.int(which(and(tex_group == tex_group[tg] + 1, 
                              seq_along(tex_group) > tg)), 
                    n = n) 
      
      stops[[j]] <- 
        nth_min.int(which(and(tex_group == tex_group[tg],
                              and(seq_along(tex_group) > tg,
                                  tex_group == dplyr::lag(tex_group, 1L, default = 0) - 1))),
                    n = n)
      j <- j + 1
    }
    data.table::data.table(starts = starts, stops = stops)
  })
}
