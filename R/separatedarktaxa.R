#' Separate dark from known taxa
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
SeparateDarkTaxa <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species
  results.dark <- c()
  results.known <- results
  for (i in sequence(length(filters))) {
    results.known <- results.known[!grepl(filters[i], results.known)]
  }
  results.dark <- setdiff(results, results.known)
  return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
}
