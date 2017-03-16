#' Function to pull scientific names from web pages
#'
#' @param URL The URL to extract names from
#' @param search.engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
GetScientificNamesFromURL <- function(URL, search.engine=0) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'fn/names_url?url=', URL, '&engine=', search.engine, sep=""))
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
  #results <- jsonlite::fromJSON(utils::URLencode(paste(GetBaseURL(), 'fn/names_url?url=', text, '&engine=', search.engine, sep="")))
  results <- jsonlite::fromJSON((paste(GetBaseURL(), 'fn/names_url?url=', utils::URLencode(text), '&engine=', search.engine, sep="")))
  return(results$scientificNames)
}

#' Resolve Scientific Names with Open Tree TNRS
#'
#' @param taxa The vector of names
#' @return A vector of corrected names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life, or the taxize package for name resolution in general.
#' @export
ResolveNamesWithOToL <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'tnrs/ot/resolve?names=', taxa.string, sep=""))
  final.names <- c()
  if(nrow(results$resolvedNames)==0) {
    warning("No names matched")
  } else {
    final.names <- results$resolvedNames$matched_name
    if(length(final.names) < length(taxa.string)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
    }
  }
  return(final.names)
}
