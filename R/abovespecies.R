#' Get above species taxon names only from a vector of taxon names.
#'
#' @param taxa A character vector of taxon names.
#' @return A character vector of taxon names above species level.
#' @examples
#' taxa_get_above_species("Vulpes_vulpes"))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
taxa_get_above_species <- function(taxa){
    taxa <- gsub(" ", "_", taxa)
    results <- unique(sapply(strsplit(taxa, "_"), "[", 1))
    return(results)
}
