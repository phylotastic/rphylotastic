% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findtrees.R
\name{taxa_get_otol_tree}
\alias{taxa_get_otol_tree}
\title{Get OToL induced subtree}
\usage{
taxa_get_otol_tree(taxa)
}
\arguments{
\item{taxa}{The vector of names, already resolved to match OToL taxa}
}
\value{
A phylo object
}
\description{
Get OToL induced subtree
}
\examples{
taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae",
          "Formica polyctena", "Tetramorium caespitum",
          "Pseudomyrmex", "Carebara diversa", "Formicinae")
phy <- taxa_get_otol_tree(taxa)
plot(phy)
}
\seealso{
\url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
}
