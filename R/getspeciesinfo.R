#' Get image metadata of a list of species
#'
#' @param species A vector of names
#' @return A data frame of image metadata(image urls, license info etc.) of species 
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or \url{https://eol.org/api/}
#' @export
GetImageDataSpecies <- function(species) {
  spec.string <- paste(species, collapse="|")
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'si/eol/get_images?species=', utils::URLencode(spec.string), sep=""))
  
  return(results$species)
}

#' Get information of a list of species 
#'
#' @param species A vector of names
#' @return A data frame of info about species
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or \url{https://eol.org/api/}
#' @export
GetInfoSpecies <- function(species) {
  spec.string <- paste(species, collapse="|")
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'sl/eol/get_links?species=', utils::URLencode(spec.string), sep=""))
  
  return(results$species)
}


