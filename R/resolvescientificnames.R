#' Resolve Scientific Names with GNR TNRS
#'
#' @param taxa The vector of names
#' @return A vector of corrected names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life, or the taxize package for name resolution in general.
#' @export
taxa_resolve_names_with_gnr <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(get_base_url(), 'tnrs/gnr/resolve?names=', taxa.string, sep=""))
  final.names <- c()
  final.names <- unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))

  if(length(final.names) < length(taxa)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
  }
  return(final.names)
}
