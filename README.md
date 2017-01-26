[![Travis-CI Build Status](https://travis-ci.org/HughParsonage/grattanReporter.svg?branch=master)](https://travis-ci.org/HughParsonage/grattanReporter)
[![codecov.io](https://codecov.io/github/HughParsonage/grattanReporter/coverage.svg?branch=master)](https://codecov.io/github/HughParsonage/grattanReporter?branch=master)

# grattanReporter
Parsing grattan reports for errors

This package assists with enforcing style in reports by the Grattan Institute, Melbourne.

In the working directory containing your report, run

```r
library(grattanReporter)
checkGrattanReport()
```
Before your report can be released you must run

```r
library(grattanReporter)
checkGrattanReport(compile = TRUE, pre_release = TRUE, release = TRUE)
```
which, provided your system meets the requirements and your report has no errors, will write a releasable PDF to a folder `RELEASE`.

