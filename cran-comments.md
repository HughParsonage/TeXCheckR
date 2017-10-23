## Test environments
* local MRAN R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.2 and dev <https://travis-ci.org/HughParsonage/TeXCheckR>
* R dev (R 3.5.0 r73560)
* win-builder (dev) <https://win-builder.r-project.org/Dx3x3pLjiQz0/00check.log>

## R CMD check results

0 errors | 0 warnings | 0 note

* I have set the test which I suspected caused the previous hang on win-builder to skip on CRAN
* The problem with pdflatex hanging did not reproduce on win-builder prior to submission. The total time for running examples and tests on both architectures was less than 30s. 
* This is a package update
