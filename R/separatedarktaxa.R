#' Separate dark from known taxa on OpenTree of Life
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @return A list containing a vector of dark names, a vector of known names, and fraction.dark
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
SeparateDarkTaxaOToL <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
  results <- unique(jsonlite::fromJSON(paste(GetBaseURL(), 'ts/all_species?taxon=', utils::URLencode(taxon), sep=""))$species)
  results.dark <- c()
  results.known <- results
  for (i in sequence(length(filters))) {
    results.known <- results.known[!grepl(filters[i], results.known)]
  }
  results.dark <- setdiff(results, results.known)
  return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
}

# #' Separate dark from known taxa on another database
# #'
# #' @param taxon A taxon to get all species for
# #' @param filters A character vector of strings to exclude
# #' @return A list containing a vector of dark names, a vector of known names, and fraction.dark
# #' @export
# SeparateDarkTaxaGenbankOLD <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.")) {
#   search.results <- rentrez::entrez_search("nuccore", term =paste0(taxon,"[Organism]"), use_history=TRUE)
#   taxa.ids <- rentrez::entrez_link(dbfrom="nuccore", db="taxonomy", web_history=search.results$web_history)
#   taxa.returns <- rentrez::entrez_summary(db="taxonomy", id=taxa.ids$links$nuccore_taxonomy)
#   results <- unique(rentrez::extract_from_esummary(taxa.returns, "scientificname"))
#   results.dark <- c()
#   results.known <- results
#   for (i in sequence(length(filters))) {
#     results.known <- results.known[!grepl(filters[i], results.known)]
#   }
#   results.dark <- setdiff(results, results.known)
#   return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
# }

#' Separate dark from known taxa on another database
#'
#' @param taxon A taxon to get all species for
#' @param filters A character vector of strings to exclude
#' @param verbose Update on how many are done
#' @return A list containing a vector of dark names, a vector of known names, and fraction.dark
#' @export
SeparateDarkTaxaGenbank <- function(taxon, filters=c("environmental", "sp\\.", "cf\\.", "uncultured"), verbose=TRUE) {
  search.results <- rentrez::entrez_search("taxonomy", term =paste0(taxon,"[subtree] AND species[Rank] "), use_history=TRUE)
  if(verbose) {
    print(paste("There are", search.results$count, "species for taxon", taxon))
  }
#  search.fetch <- entrez_fetch(db="taxonomy", web_history=search.results$web_history, rettype="xml", parsed=TRUE)
  taxa.returns <- rentrez::entrez_summary(db="taxonomy", web_history=search.results$web_history, version=c("1.0"))
  if(verbose) {
    print(paste("Initially found", length(taxa.returns), "for taxon", taxon))
  }
  all.taxa.returns <- taxa.returns
  loop.count <- 1
  while(length(taxa.returns)==10000) {
    taxa.returns <- rentrez::entrez_summary(db="taxonomy", web_history=search.results$web_history, version=c("1.0"),retstart=(loop.count*10000)) #it's 0 indexed, so no need to do +1 for retstart
    loop.count <- loop.count + 1
    all.taxa.returns <- c(all.taxa.returns, taxa.returns)
    if(verbose) {
      print(paste("Found", length(taxa.returns), "more for taxon", taxon, "with",length(all.taxa.returns),"species in total, which represents", 100*length(all.taxa.returns) / search.results$count, "percent of all"))
    }
  }
  results <- unique(rentrez::extract_from_esummary(taxa.returns, "ScientificName"))
  results.dark <- c()
  results.known <- results
  for (i in sequence(length(filters))) {
    results.known <- results.known[!grepl(filters[i], results.known)]
  }
  results.dark <- setdiff(results, results.known)
  return(list(dark=results.dark, known=results.known, fraction.dark = length(results.dark)/length(results)))
}
