#' Keep the first part of the binomial from a vector of taxon names that includes species binomial names
#'
#' @param taxa A character vector of taxon names.
#' @return A character vector of lineage names above the species level.
#' @examples
#' taxa_toss_binomials("Vulpes_vulpes")
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxa_toss_binomials <- function(taxa){
    taxa <- gsub(" ", "_", taxa)
    results <- unique(sapply(strsplit(taxa, "_"), "[", 1))
    return(results)
}
