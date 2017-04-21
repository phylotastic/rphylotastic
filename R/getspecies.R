#' Get all species from a taxon
#'
#' @param taxon A taxon to get all species for
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
GetSpeciesFromTaxon <- function(taxon) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species
  tmp.file <- paste(tempdir(), "/tmp.tre", sep="")
  cat(results, file=tmp.file)
  tree <- ape::reorder.phylo(ape::collapse.singles(methods::as(phylobase::readNewick(tmp.file), "phylo")))
  return(tree)
}
