
check_timing <- function(comment = "", date = Sys.time()){
  stopifnot(dir.exists("./timings/"), 
            dir.exists("./tests/testthat/SchoolFunding/"), 
            requireNamespace("data.table", quietly = TRUE))
  library(data.table)
  checkCircuiBreaker <- function(){
    checkGrattanReport("./tests/testthat/SchoolFunding/")
  }
  timed <- microbenchmark::microbenchmark(checkCircuiBreaker(), times = 30L)
  timed %>%
    setDT %>%
    .[, date := date] %>%
    .[, comment := comment] %>%
    fwrite("./timings/checkGrattanReport.csv", append = TRUE)
}
