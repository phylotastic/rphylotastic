% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separatedarktaxa.R
\name{taxon_separate_dark_taxa_using_otol}
\alias{taxon_separate_dark_taxa_using_otol}
\title{Separate dark from known taxa on OpenTree of Life}
\usage{
taxon_separate_dark_taxa_using_otol(
  taxon,
  filters = c("environmental", "sp\\\\.", "cf\\\\.", "uncultured", "unidentified",
    " clone", " enrichment")
)
}
\arguments{
\item{taxon}{A taxon to get all species for}

\item{filters}{A character vector of strings to exclude}
}
\value{
A list containing a vector of dark names, a vector of known names, and fraction.dark
}
\description{
Separate dark from known taxa on OpenTree of Life
}
\seealso{
\url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
}
