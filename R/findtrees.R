#' Get OToL induced subtree
#'
#' @param taxa The vector of names, already resolved to match OToL taxa
#' @return A phylo object
#' @examples
#' taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae",
#'           "Formica polyctena", "Tetramorium caespitum",
#'           "Pseudomyrmex", "Carebara diversa", "Formicinae")
#' phy <- taxa_get_otol_tree(taxa)
#' plot(phy)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxa_get_otol_tree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(get_base_url(), 'gt/ot/get_tree?taxa=', taxa.string, sep=""))$newick
  tmp.file <- paste(tempdir(), "/tmp.tre", sep="")
  cat(results, file=tmp.file)
  tree <- ape::reorder.phylo(ape::collapse.singles(methods::as(phylobase::readNewick(tmp.file), "phylo")))
  return(tree)
}


#' Get phylomatic subtree
#'
#' @param taxa The vector of names, already resolved
#' @return A phylo object
#' @examples
#' phy <- taxa_get_phylomatic_tree(c("Panthera leo", "Panthera onca",
#'          "Panthera tigris", "Panthera uncia"))
#' plot(phy)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the interface of phylomatic \url{http://phylodiversity.net/phylomatic/}
#' @export
taxa_get_phylomatic_tree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(get_base_url(), 'gt/pm/get_tree?taxa=', taxa.string, sep=""))

  tree <- ape::read.tree(text=results$newick)

  return(tree)
}


#' Get taxonomic tree from the NCBI
#'
#' @param taxa The vector of names, already resolved
#' @return A phylo object
#' @examples
#' taxa <- c("Setophaga striata", "Setophaga magnolia",
#'      "Setophaga angelae", "Setophaga plumbea",
#'      "Setophaga virens")
#' phy <- taxa_get_taxonomic_tree(taxa)
#' plot(phy)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the interface of phyloT \url{http://phylot.biobyte.de/}
#' @export
taxa_get_taxonomic_tree <- function(taxa) {
  taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
  results <- jsonlite::fromJSON(paste(get_base_url(), 'gt/pt/get_tree?taxa=', taxa.string, sep=""))

  tree <- ape::read.tree(text=results$newick)

  return(tree)
}
