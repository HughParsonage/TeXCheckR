
locate_punct_after_footcite <- function(chars, singular = TRUE) {
  v_match <- function(needle, haystack, nomatch = 0L, singular = TRUE) {
    sieved <- which(haystack == needle[1L]) 
    for (i in seq.int(1L, length(needle) - 1L)) {
      sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
    }
    if (singular) {
      # No 'footcites'
      sieved <- sieved[haystack[sieved + i + 1L] != "s"]
    }
    
    sieved
  }
  
  footcite_starts <- v_match(strsplit("\\footcite", split = "", fixed = TRUE)[[1L]], chars) 
  nchar_footcite <- nchar("footcite")
  
  L <- 
    lapply(footcite_starts, function(i) {
      # Let s denote the current character
      # being looked at
      j <- i
      s <- chars[j]
      while (s != "}") {
        j <- j + 1L
        s <- chars[j]
      }
      for (k in 1:200) {
        if (k > 1L && chars[j + k] == "-") {
          return(list(char = -1L, punct = -1L))
        }
        if (k == 1L && chars[j + k] %fin% punctuation) {
          break
        }
        if (grepl("\\s", chars[j + k])) {
          next
        }
        
        return(list(char = -1L, punct = -1L))
      }
      
      list(char = i, punct = j + k)
    })
  
  
  rbindlist(L)
}


