#' Compare two phylogenetic trees
#'
#' @param trees A character vector of two newick trees
#' @return A named list with are_same_tree property set to either TRUE or FALSE
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export

newick_compare <- function(trees) {
  url <- "http://phylo.cs.nmsu.edu:5006/phylotastic_ws/compare_trees"
  body <- list(tree1_nwk = trees[1], tree2_nwk = trees[2])
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result)
}
