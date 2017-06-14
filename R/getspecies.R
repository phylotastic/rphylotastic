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

#' Get all species filtered by country from a taxon 
#'
#' @param taxon A taxon to get subset of species for
#' @param country A country name where species of the input taxon are established.
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
GetCountrySpeciesFromTaxon <- function(taxon, country, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'ts/country_species?taxon=', utils::URLencode(taxon), '&country=', utils::URLencode(country), sep=""))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}
