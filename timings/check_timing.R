
check_timing <- function(comment = ""){
  stopifnot(dir.exists("./timings/"), 
            dir.exists("./tests/testthat/SchoolFunding/"))
  checkCircuiBreaker <- function(){
    checkGrattanReport("./tests/testthat/SchoolFunding/")
  }
  timed <- microbenchmark::microbenchmark(checkCircuiBreaker(), times = 30L)
  timed %>%
    setDT %>%
    .[, date := Sys.time()] %>%
    .[, comment := comment] %>%
    fwrite("./timings/checkGrattanReport.csv")
}
