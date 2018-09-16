
locate_footcite_punctuation <- function(parsed_doc = NULL, singular = TRUE, tex_lines = NULL) {
  if (is.null(parsed_doc)) {
    parsed_doc <- parse_tex(tex_lines)
  }
  chars <- .subset2(parsed_doc, "char")
  v_match <- function(needle, haystack, nomatch = 0L, singular = TRUE) {
    sieved <- which(haystack == needle[1L]) 
    for (i in seq.int(1L, length(needle) - 1L)) {
      sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
    }
    if (singular) {
      # Don't match 'footcites'
      sieved <- sieved[haystack[sieved + i + 1L] != "s"]
    }
    
    sieved
  }
  
  if (singular) {
    footcite_starts <- v_match(strsplit("\\footcite", split = "", fixed = TRUE)[[1L]], chars) 
  } else {
    footcite_starts <- v_match(strsplit("\\footcites", split = "", fixed = TRUE)[[1L]], chars) 
  }
  
  L <- 
    lapply(footcite_starts, function(i) {
      # Let s denote the current character
      # being looked at
      j <- i + 9L
      s <- chars[j]
      
      # Find the closing brace: 
      #  - if singular, just the next closing brace 
      #    (no need to parse; contents never )
      TeX_group <- 0L
      opt_TeX_group <- 0L
      while (OR(s != "}" || TeX_group || opt_TeX_group,  # rely on 0 => FALSE 1+ => TRUE
                if (singular) {
                  FALSE 
                } else {
                  # if footcites, continue if 
                  # we're just going to the next key
                  OR(chars[j + 1L] == "{",
                     chars[j + 1L] == "[")
                })) {
        # Sys.sleep(1)
        # cat(s)
        if (opt_TeX_group) {
          switch(s,
                 "{" = {
                   # We've encountered a group and need to monitor until we're down
                   TeX_group <- TeX_group + 1L
                 },
                 "}" = {
                   # This should be safe because we know
                   # that "}" during TeX group == 0
                   # will break
                   TeX_group <- TeX_group - 1L
                 },
                 "[" = {
                   opt_TeX_group <- opt_TeX_group + 1L
                 },
                 "]" = {
                   opt_TeX_group <- opt_TeX_group - 1L
                 })
        } else {
          switch(s,
                 "[" = {
                   opt_TeX_group <- opt_TeX_group + 1L
                 },
                 "]" = {
                   opt_TeX_group <- opt_TeX_group - 1L
                 })
        }
        
        j <- j + 1L
        s <- chars[j]
      }
      
      
      for (k in 1:200) {
        if (k > 1L && chars[j + k] == "-") {
          return(NULL)
        }
        if (k == 1L && chars[j + k] %fin% punctuation) {
          break
        }
        if (grepl("\\s", chars[j + k])) {
          next
        }
        
        return(NULL)
      }
      
      list(start = i, char_no = j + k)
    })
  
  bound <- rbindlist(L)
  # Faster than setkey and already known to be sorted
  setattr(bound, "sorted", "char_no")
  
  # CRAN NOTE avoidance
  column <- NULL
  parsed_doc[bound, on = "char_no"][column > 1L]
}




