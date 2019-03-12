#' Reduce species from a vector of taxon names. It will keep the most inclusive lineage above the species level.
#'
#' @param taxa A character vector of taxon names.
#' @return A character vector of lineage names above the species level.
#' @examples
#' taxon_reduce_species("Vulpes_vulpes"))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxa_reduce_species <- function(taxa){
    taxa <- gsub(" ", "_", taxa)
    results <- unique(sapply(strsplit(taxa, "_"), "[", 1))
    return(results)
}
