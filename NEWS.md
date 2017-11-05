# TeXCheckR 0.5.0
* Allow `fread_bib` to optionally retain braces in `value`.


# TeXCheckR 0.4.3
* Use `hunspell`'s ignore option when a dictionary addition is not respected
* Fix spurious unit test broken under new `hunspell`.
* Fix issue with `check_footnote_typography` where the RStudio pops to the end of the first footnote, not the first wrong footntoe. Thanks to CC for reporting.

# TeXCheckR 0.4.2
* Fixed `inputs_of` to select `\include` and `\inputs` only
* Export `inputs_of`

# TeXCheckR 0.4.1
* Fixed an issue whereby `pdflatex` may not run in certain environments (in particular CRAN where it caused an egregious hanging of the package queue). Tests now skip in such cases. 

# TeXCheckR 0.4.0
* Major internal functions `parse_tex`, `extract_mandatory_LaTeX_argument`, and `extract_optional_LaTeX_argument`.

# TeXCheckR 0.3.3

* `check_footnote_typography` can now provide the precise location of incorrect punctuation, instead of just reporting the head of footnote.
* Use `extract_LaTeX_argument` now can handles multiline arguments.
* Added a `NEWS.md` file to track changes to the package.



