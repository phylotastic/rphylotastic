#' Get OToL induced subtree
#'
#' @param taxa The vector of names, already resolved to match OToL taxa
#' @return A phylo object
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the rotl package, another interface to Open Tree of Life
#' @export
GetOToLTree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'gt/ot/get_tree?taxa=', taxa.string, sep=""))$newick
  tmp.file <- paste(tempdir(), "/tmp.tre", sep="")
  cat(results, file=tmp.file)
  tree <- ape::reorder.phylo(ape::collapse.singles(methods::as(phylobase::readNewick(tmp.file), "phylo")))
  return(tree)
}


#' Get phylomatic subtree
#'
#' @param taxa The vector of names, already resolved
#' @return A newick string
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the interface of phylomatic \url{http://phylodiversity.net/phylomatic/}
#' @export
GetPhylomaticTree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'gt/pm/get_tree?taxa=', taxa.string, sep=""))
  
  tree <- results$newick

  return(tree)
}


#' Get taxonomic tree from the NCBI
#'
#' @param taxa The vector of names, already resolved
#' @return A newick string
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md} or the interface of phyloT \url{http://phylot.biobyte.de/}
#' @export
GetTaxonomicTree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(GetBaseURL(), 'gt/pt/get_tree?taxa=', taxa.string, sep=""))
  
  tree <- results$tree_newick

  return(tree)
}
