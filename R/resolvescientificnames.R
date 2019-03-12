#' Resolve Scientific Names with Open Tree TNRS
#'
#' @param taxa The vector of names
#' @return A vector of corrected names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @examples
#' my.species.raw <- c("Formica polyctena", "Formica exsectoides", "Farmica pacifica")
#' my.species.corrected <- taxa_resolve_names_with_otol(my.species.raw)
#' print(my.species.corrected)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life, or the taxize package for name resolution in general.
#' @export
taxa_resolve_names_with_otol <- function(taxa) {
    # we used GET previously, but POST is faster and has no limits to taxon names input:
    taxa.string <- paste(taxa, collapse='", "')
    postcall <- paste0('{"scientificNames": ["', taxa.string, '"], "fuzzy_match":true}')
    # results <- jsonlite::fromJSON(paste0(get_base_url(), 'gt/ot/get_tree?taxa=', taxa.string))$newick
    # curl -X POST "https://phylo.cs.nmsu.edu/phylotastic_ws/gt/ot/tree" -H "content-type:application/json" -d '{"taxa": ["Setophaga striata","Setophaga magnolia","Setophaga angelae","Setophaga plumbea","Setophaga virens"]}'
    postcall <- paste0("curl -X POST '", get_base_url(), "tnrs/ot/names' -H 'content-type:application/json' -d '", postcall, "'")
    results <- jsonlite::fromJSON(system(postcall, intern=TRUE))
    final.names <- unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))
    if(length(final.names) < length(taxa)) {
      message("Fewer names were matched than were given; missing taxa were dropped.")
    }
    return(final.names)
}
# i=11
# taxa.string <- utils::URLencode(paste(taxa[i], collapse="|"))
# results <- jsonlite::fromJSON(paste0(get_base_url(), 'tnrs/ot/resolve?names=', taxa.string))
# results
# length(results$resolvedNames) > 0
# unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))
#' Resolve Scientific Names with GNR TNRS
#'
#' @param taxa The vector of names
#' @return A vector of correct names. THE ORDER MAY NOT CORRESPOND TO YOUR INPUT ORDER.
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life, or the taxize package for name resolution in general.
#' @details Mispelled or incorrect names will be dropped.
#' @export
taxa_resolve_names_with_gnr <- function(taxa) {
    final.names <- vector(mode = "character", length = length(taxa))
    progression <- utils::txtProgressBar(min = 0, max = length(taxa), style = 3)
    for(i in seq(length(taxa))){
        taxa.string <- utils::URLencode(paste(taxa[i], collapse="|"))
         results <- jsonlite::fromJSON(paste0(get_base_url(), 'tnrs/gnr/resolve?names=', taxa.string))
         if(length(results$resolvedNames) > 0){
             final.names[i] <- unlist(lapply(results$resolvedNames$matched_results, "[[", "matched_name"))
         }
         utils::setTxtProgressBar(progression, i)
    }
    cat("\n") # just to make the progress bar look better
    final.names <- final.names[final.names !=""]
    if(length(final.names) < length(taxa)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
    }
    return(final.names)
}

#' Convert common names to scientific names.
#'
#' @param taxa A character vector of common names. Binomials can be spaced with underscore or white space.
#' @param service Which service to use: NCBI, ITIS, or TROPICOS
#' @param multiple  If TRUE, then the service will return multiple matches (if available) for each common name in the input list.
#' @return A vector of scientific names. Output order may not correspond to input order.
#' @seealso taxize package for name resolution in general and its sci2comm function.
#' @examples
#' taxa <- c("blue_whale", "swordfish", "killer whale")
#' scientific <- taxa_common_to_scientific(taxa)
#' print(scientific)
#' @export
taxa_common_to_scientific <- function(taxa, service="NCBI", multiple=FALSE) {
  # taxa <- c("Osprey", "House sparrow", "Mallard duck", "American Robin")
  # taxa <- c("venus flytrap", "pitcher plant", "cobra lily", "sundew")
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  service <- tolower(service)
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'cs/', service, '/get_scientific_names?commonnames=', taxa.string))
  final.names <- as.character(sapply(results$result$matched_names, "[[", "scientific_name"))
  names(final.names) <- tolower(sapply(results$result$matched_names, "[[", "common_name"))
  if(length(final.names) < length(taxa)) {
      warning("Fewer names were found than were given; missing taxa were dropped.")
  }
  return(gsub(" ", "_", final.names))
}
