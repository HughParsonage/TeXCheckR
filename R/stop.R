

stop <- function(...) {
  if (getOption("TeXCheckR.no_stopping", FALSE)) {
    return(NULL)
  }
  eval.parent(substitute(base::stop(...)))
}

