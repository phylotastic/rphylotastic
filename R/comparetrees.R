#' Compare two phylogenetic trees
#'
#' @param trees A multiphylo object of two newick trees
#' @return A named list with are_same_tree property set to either TRUE or FALSE
#' @examples
#' are_same <- newick_compare(c(ape::rcoal(5), ape::rcoal(5)))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export

newick_compare <- function(trees) {
  url <- "http://phylo.cs.nmsu.edu:5006/phylotastic_ws/compare_trees"
  if(length(trees)!=2) {
    stop("Can only have two trees as input")
  }
  body <- list(tree1_nwk = ape::write.tree(trees[1]), tree2_nwk = ape::write.tree(trees[2]))
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result$are_same_tree)
}
