#' Resolve Scientific Names with GNR TNRS
#'
#' @param taxa The vector of names
#' @return A vector of correct names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life, or the taxize package for name resolution in general.
#' @details Mispelled or incorrect names will be dropped.
#' @export
taxa_resolve_names_with_gnr <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'tnrs/gnr/resolve?names=', taxa.string))
  final.names <- c()
  final.names <- unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))

  if(length(final.names) < length(taxa)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
  }
  return(final.names)
}

#' Convert common names to scientific names
#'
#' @param taxa The vector of names
#' @param service Which service to use: NCBI, ITIS, or TROPICOS
#' @param multiple  If TRUE, then the service will return multiple matches (if available) for each common name in the input list.
#' @return A vector of correct names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @seealso taxize package for name resolution in general and its sci2comm function.
#' @examples
#' taxa <- c("blue whale", "swordfish","killer whale")
#' common <- taxa_convert_common_to_scientific(taxa)
#' print(common)
#' @export
taxa_convert_common_to_scientific <- function(taxa, service="NCBI", multiple=FALSE) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  service <- tolower(service)
  results <- jsonlite::fromJSON(paste0(rphylotastic:::get_base_url(), 'cs/', service, '/get_scientific_names?commonnames=', taxa.string))
  final.names <- c()
  final.names <- unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))

  if(length(final.names) < length(taxa)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
  }
  return(final.names)
}
