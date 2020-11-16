Development page for TeXCheckR. See also
<a href="https://github.com/hughparsonage/grattanReporter" class="uri">https://github.com/hughparsonage/grattanReporter</a>

``` r
# install.packages("TeXCheckR")
library(TeXCheckR)
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.0.2

``` r
library(pander)
```

    ## Warning: package 'pander' was built under R version 4.0.3

``` r
library(magrittr)
```

``` r
eg <- system.file("extdata", "readme.tex", package = "TeXCheckR")
```

``` latex
\documentclass{article}

\begin{document}

I'd like an en dash - now.

Quotes need to be entered 'precisely'.

Footnotes can't have spaces before. \footnote{And should be full sentences}

We shouldn't refer to section 1 by typing out the literal cross-reference.

\section{Consitent labels are a good custom}\label{good-customs}

And English spacing dictates sentence spacing HERE. Otherwise the spacing looks strange.

Here the mismatched parentheses are obvious. (But it's not always the case.

We want to avoid this formatting: 10 million payments of $5 would total $50 million. 
And \LaTeX{} won't raise an error.

\end{document}
```

``` r
check_dashes(eg)
```

    ## 
    ## Dash likely masquerading as hyphen. Use -- for a dash.
    ## <U+2716> 5: As always, visually check the result in the PDF.I'd like an en dash - now.
    ## 
    ## IMPORTANT: make sure you are replacing a hyphen with two hyphens, not a unicode dash  –

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Single hyphen surrounded by spaces. This is likely a hyphen masquerading as dash. Use -- for a dash.
    ## 
    ## IMPORTANT: make sure you are replacing a hyphen with two hyphens, not a unicode dash  –
    ## If you're not sure, reenter as two hyphens from the keyboard (rather than just appending a hyphen at the end). As always, visually check the result in the PDF.

``` r
check_quote_marks(eg)
```

    ## 
    ## Closing quote used at beginning of word. Use a backtick for an opening quote, e.g. The word `ossifrage' is quoted.
    ## <U+2716> 7: Quotes need to be entered 'precisely'.
    ##                                 ^

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Closing quote used at beginning of word. Use a backtick for an opening quote, e.g. The word `ossifrage' is quoted.

``` r
check_footnote_typography(eg)
```

    ## 
    ## Footnote does not end with full stop.
    ## <U+2716> 9: 
    ## \footnote
    ##          {And should be full sentences}

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Footnote does not end with full stop.

``` r
check_literal_xrefs(eg)
```

    ## 
    ## Hard-coded xref in document.
    ## <U+2716> 11: We shouldn't refer to section 1 by typing out the literal cross-reference.
    ## All xrefs need to use \Cref or \Vref (or \Chapref for cross-references to chapters). If you need to use this phrase, you can use a non-breaking space e.g. 'Section~81 of the Constitution.

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Hard-coded xref in document. All xrefs need to use \Cref or \Vref (or \Chapref for cross-references to chapters).

``` r
check_labels(eg)
```

    ## 
    ## \label used without prefix.
    ## <U+2716> 13: \section{Consitent labels are a good custom}\label{good-customs}
    ## Use fig: tbl: box: chap: subsec: paragraph: rec: fn: in every label.

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Each \label must contain a prefix.

``` r
TeXCheckR:::check_sentence_ending_periods(eg)
```

    ## 
    ## Capital letter ends sentence, but sentence-ending period mark absent.
    ## <U+2716> 15: And English spacing dictates sentence spacing HERE. Otherwise the spacing looks strange.
    ## Sentences which end with a capital letter need to be signalled with a sentence-ending period. (\@.)

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Sentences which end with a capital letter need to be signalled with a sentence-ending period. (\@.)

``` r
TeXCheckR:::check_unclosed_parentheses(eg)
```

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : 17 contains parenthesis that does not close.

``` r
check_escapes(eg)
```

    ## 
    ## Unescaped $.
    ## <U+2716> 19: We want to avoid this formatting: 10 million payments of $5 would total
    ##                                                                 ^
    ## If you meant to print a dollar sign, use \$. If you want to use math-mode, use \(...\), not $...$ .

    ## Error in rmarkdown::render("C:/Users/hughp/Documents/TeXCheckR/README.Rmd", : Unescaped $. If you meant to print a dollar sign, use \$. If you want to use math-mode, use \(...\), not $...$ .
