#' Get all species from a taxon
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
GetSpeciesFromTaxon <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}
