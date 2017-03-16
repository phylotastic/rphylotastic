#' Get OToL induced subtree
#'
#' @param taxa The vector of names, already resolved to match OToL taxa
#' @return A phylo object
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
GetOToLTree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'gt/ot/get_tree?taxa=', taxa.string, sep=""))$newick
  return(ape::read.tree(text=results))
}
