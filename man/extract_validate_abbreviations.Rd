% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_valid_abbreviations.R
\name{extract_validate_abbreviations}
\alias{extract_validate_abbreviations}
\alias{extract_valid_abbrevations}
\title{Extract valid abbreviations and initialisms}
\usage{
extract_validate_abbreviations(lines)
}
\arguments{
\item{lines}{Lines to extract}
}
\value{
Character vector of abbreviations of the form (ABC)
}
\description{
Extracts abbreviations which are preceded by the full text (\emph{e.g.} 'The Quebec Xylophone Enterprise Foundation (QXEF)').
}
\details{
Only 'valid' abbreviations are extracted, viz. those abbreviations of the form \code{(ABC)} where the first letters 
of the preceding words 
(excluding some common words like \code{of}, \code{and}, etc.) are 'a', 'b', 'c'.
}
