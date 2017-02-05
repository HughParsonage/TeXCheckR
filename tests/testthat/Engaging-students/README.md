# grattex
This is the repository containing the configuration files for reports in the style of the [Grattan Institute, Melbourne](https://grattan.edu.au/).
Staff starting a new report should do the following:

  1. Login as the `grattaninstitute` super user in [github.com](https://github.com) and [sharelatex.com](https://sharelatex.com).
  2. Copy `https://github.com/HughParsonage/grattex` to your clipboard
  2. Visit https://github.com/new/import
  3. Paste `https://github.com/HughParsonage/grattex` into the `Your old repository’s clone URL`.
  3. Choose an evocative name for your repository under `Name`
  4. Select Private
  5. Click `Begin Import`.
  6. After the import is complete, follow the link to the report.
  6. Click the Settings Tab.
  6. On the left, click `Collaborators`. 
  6. Add `HughParsonage` and any other authors as desired.
  7. Visit [sharelatex.com](https://sharelatex.com).
  7. Click `New Project > Import from GitHub`.
  7. Locate the repository you just created, and click `Import to ShareLaTeX`. 
  7. If ShareLaTeX fails to compile, this is a bug. Otherwise, proceed. (The first compilation should take several minutes, resulting in a document around 150 pages.)
  7. At the top right, click `Share`.
  7. Add collaborators as desired. 

## Changelog

### 2016-01-17
* New option 'submission'.

### 2016-01-06
* Add `\Chapref` and friends for neater hyperlinks to chapters
* Add `\ie` `\eg` `\etc` macros.

### 2016-12-19
* Allow long URLs in bibliography, line-breaking at width #36
* Box footnotes are now non-italic #43
* New command, `\doublecolumnfigure` now used #46
* Documentation moved to `./doc/`

### 2016-11-19

New features:
* New option `embargoed` enables a command `\EmbargoDate` which prints an Embargo mark on the title page and in the headers #32

* Patched bug in `cleveref` where `varioref`'s phrase ``on the previous page'' is never used. #39
* `[t]` floats have captions aligned with the baseline. 076df622e7fec025382b804e1b809319aef2fe11
* KOMA-warnings' advice about `footheight`, `headheight` etc has been accepted. #40
* Glue component of space between footnote area and text has been reduced to 14pt (which is closer to the current leading). aec03a693b0aad85daa03ceef16316ef855b5b4a
* Manual specification of `\textfloatsep` has now been dropped in favour of the default
* Allow citations in acknowledgements #37


