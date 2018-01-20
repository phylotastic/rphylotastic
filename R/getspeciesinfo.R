#' Get image metadata of a list of species
#'
#' @param species A vector of names
#' @return A data frame of image metadata(image urls, license info etc.) of species
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or \url{https://eol.org/api/}
#' @export
species_get_image_data <- function(species) {
  spec.string <- paste(species, collapse="|")
  results <- jsonlite::fromJSON(paste(get_base_url(), 'si/eol/get_images?species=', utils::URLencode(spec.string), sep=""))

  return(results$species)
}

#' Get information of a list of species
#'
#' @param species A vector of names
#' @return A data frame of info about species
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or \url{https://eol.org/api/}
#' @export
species_get_info <- function(species) {
  spec.string <- paste(species, collapse="|")
  results <- jsonlite::fromJSON(paste(get_base_url(), 'sl/eol/get_links?species=', utils::URLencode(spec.string), sep=""))

  return(results$species)
}
