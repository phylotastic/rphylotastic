#' Get all species from a taxon
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @examples
#' print(taxon_get_species("Vulpes"))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(get_base_url(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}

#' Get all species filtered by country from a taxon
#'
#' @param taxon A taxon to get subset of species that are established in a particular country
#' @param country A country name where species of the input taxon are established.
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species_and_country <- function(taxon, country, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(get_base_url(), 'ts/country_species?taxon=', utils::URLencode(taxon), '&country=', utils::URLencode(country), sep=""))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}

#' Get all species that have genome sequence in NCBI from a taxon
#'
#' @param taxon A taxon to get subset of species having genome sequence
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species_with_genome <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(get_base_url(), 'ts/ncbi/genome_species?taxon=', utils::URLencode(taxon), sep=""))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}
