#' Separate dark from known taxa on OpenTree of Life
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
SeparateDarkTaxaOToL <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species
  results.dark <- c()
  results.known <- results
  for (i in sequence(length(filters))) {
    results.known <- results.known[!grepl(filters[i], results.known)]
  }
  results.dark <- setdiff(results, results.known)
  return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
}

#' Separate dark from known taxa on another database
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A vector of names
#' @export
SeparateDarkTaxaGenbank <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  search.results <- rentrez::entrez_search("nuccore", term =paste0(taxon,"[Organism]"), use_history=TRUE)
  taxa.ids <- rentrez::entrez_link(dbfrom="nuccore", db="taxonomy", web_history=search.results$web_history)
  taxa.returns <- rentrez::entrez_summary(db="taxonomy", id=taxa.ids$links$nuccore_taxonomy)
  results <- rentrez::extract_from_esummary(taxa.returns, "scientificname")
  results.dark <- c()
  results.known <- results
  for (i in sequence(length(filters))) {
    results.known <- results.known[!grepl(filters[i], results.known)]
  }
  results.dark <- setdiff(results, results.known)
  return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
}
