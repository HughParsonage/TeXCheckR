

# TeXCheckR 0.6.0

## Enhancements:
* `validate_bibliography` now errors if url fields contain escaped symbols.
* `check_dashes` now do not error if an emdash occurs in a protasis within an list, or if requested.
* `check_spelling` now obeys a directive `% ignore_spelling_in_file: <file.tex>` for files where the spelling may be ignored (such as tables with symbols, jargon, or esoteric nomenclature).

## Bug fixes:
* `check_unclosed_parentheses` no longer errors or panics over unclosed parentheses occurring within optional arguments to `\begin{enumerate}` such as `label` constructions.

## Other 
* Test involving `readr` did not properly check for it being installed


# TeXCheckR 0.5.1
* Allow `fread_bib` to optionally retain braces in `value`.
* New functions:
  * `split_report` to split a report into chapters
  * `minimal_bib` to produce a minimal bibliography from a spun document
  * `veto_sic` to locate and remove words preceding `[sic]` (especially during spell checks)
  * `strip_comments` is now exported
  * `read_tex_document` to take the root file of a LaTeX document and read the entire document (including the contents `\input` and `\include`)
  
  * (Unexported) `check_unclosed_parentheses` to check (some) instances of unclosed or unopened parentheses
* `stringi` and `readr` have been moved to Suggests to minimize compile times (as on Travis-CI)
* `check_spelling` now skip words 'preceding' an editorial `[sic]`.
* `check_spelling` tries to fill skipped commands with whitespace equal to its contents to replace (to provide more accurate positions if there is an error).
* `halt` in `report_error` now provides an option to not halt if a check fails.
* Experimental utility functions `fill_nth_LaTeX_argument` and `locate_nth_LaTeX_argument`. 
* `extract_valid_abbrevations` (and thus `check_spelling`) considers the word `on` optional when backtracking.
* `check_xrefs` defaults now permits lowercase forms. By default, consistency is enforced (whether upper or lower).
* `separate_sentences` gains a `hanging_footnotes` argument, indenting footnotes if set to `TRUE`.
* Add some proper-nouns to the dictionary.

* Bug fixes:
  * check labels now test for spaces rather than check dashes
  * `lint_bib` does not add commas after `@string` fields.
  * `inputs_of` does not emit arcane warning if multiple `\end{document}`s exist.
  * `check_escapes` do not check content within `tikzpicture` environment or within `\url` commands.
  * `check_dashes` does not check display equation lines.
  * `check_labels` does not check that `caption` commands in the preamble are associated with a label.
  Similarly, it does not check the labels associated with `\addchap` and `\chapter` unless they have an opening brace. (For example, within `\renewcommand`). 
  * `check_quote_marks` does not check quote marks beyond `\end{document}`
  * `check_sentence_ending_periods` does not check the contents of `\hl{` (which may be used to markup content during editing).
  * `check_spelling` honours non-default settings of `dict_lang` in files through `\input` or `\include`, does not check the contents of `tikz` environements, does not look at the optional arguments of `\printbibliography`
  * `check_consecutive_words` no longer requires `grattan.cls` to run. Thanks to @jonocarroll for reporting.
  * `check_footnote_typography` no longer falsely claims a footnote lacks a terminal full stop when it ends in a list like `enumerate` or `itemize` (and the terminal item ends with a full stop).
  
* Internal:
  * `readr`, `stringi` are now in Suggests to reduce compile time (especially on Travis-CI)

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



