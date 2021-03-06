% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa_toss_binomials.R
\name{taxa_toss_binomials}
\alias{taxa_toss_binomials}
\title{Keep the first part of the binomial from a vector of taxon names that includes species binomial names}
\usage{
taxa_toss_binomials(taxa)
}
\arguments{
\item{taxa}{A character vector of taxon names.}
}
\value{
A character vector of lineage names above the species level.
}
\description{
Keep the first part of the binomial from a vector of taxon names that includes species binomial names
}
\examples{
taxa_toss_binomials("Vulpes_vulpes")
}
\seealso{
\url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
}
