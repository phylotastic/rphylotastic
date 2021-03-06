#' Get all species from a taxon from Open Tree of Life taxonomy.
#'
#' @param taxon A character vector of length 1. Specify the taxon name to get all species for. If vector is longer than 1, it will only take the first element and ignore all other names.
#' @param filters A character vector of strings to exclude.
#' @return A character vector of species names.
#' @examples
#' print(taxon_get_species("Vulpes"))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  if(length(taxon) > 1){
    message("there is more than one name in taxon argument, only the first name will be searched")
  }
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'ts/ot/all_species?taxon=', utils::URLencode(taxon)))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}

#' Get all species filtered by country from a taxon
#'
#' @param taxon A character vector of length 1. Specify the taxon name to get a subset of species that are established in a particular country
#' @param country A country name where species of the input taxon are established.
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species_from_country <- function(taxon, country, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'ts/ot/country_species?taxon=', utils::URLencode(taxon), '&country=', utils::URLencode(country)))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}

#' Get all species that have genome sequence in NCBI from a taxon
#'
#' @param taxon A character vector of length 1. Specify the taxon name to get a subset of species having genome sequence
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxon_get_species_with_genome <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'ts/ncbi/genome_species?taxon=', utils::URLencode(taxon)))$species
  for (i in sequence(length(filters))) {
    results <- results[!grepl(filters[i], results)]
  }
  return(results)
}
