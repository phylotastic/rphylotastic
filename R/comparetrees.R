#' Compare two phylogenetic trees
#'
#' @param trees A character vector of two newick trees
#' @return A named list with are_same_tree property set to either TRUE or FALSE
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export

CompareTrees <- function(trees) {
  library(httr)
  url <- "http://phylo.cs.nmsu.edu:5006/phylotastic_ws/compare_trees"
  body <- list(tree1_nwk = trees[1], tree2_nwk = trees[2])
  response <- POST(url, body = body, encode = "json")
  result <- content(response,"parsed")	
  
  return(result)
}


