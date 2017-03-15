#' Function to pull scientific names from web pages
#'
#' @param URL The URL to extract names from
#' @param search.engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
GetScientificNamesFromURL <- function(URL, search.engine=0) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'names_url?url=', URL, '&engine=', search.engine, sep=""))
  return(results$scientificNames)
}

#' Function to pull scientific names from text
#'
#' @param text The text string to extract names from
#' @param search.engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
GetScientificNamesFromText <- function(text, search.engine=0) {
  results <- jsonlite::fromJSON(utils::URLencode(paste(GetBaseURL(), 'names_url?url=', text, '&engine=', search.engine, sep="")))
  return(results$scientificNames)
}
