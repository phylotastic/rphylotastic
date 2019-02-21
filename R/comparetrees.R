#' Compare two phylogenetic trees
#'
#' @param phylo1 A multiphylo object with two trees to be compared between them or a single phylo object to be compared to phylo2
#' @param phylo2 A phylo object to be compared to phylo1
#' @return A named list with are_same_tree property set to either TRUE or FALSE
#' @examples
#' are_same <- phylo_compare(c(ape::rcoal(5), ape::rcoal(5)))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export

phylo_compare <- function(phylo1, phylo2 = NULL) {
  url <- paste0(get_base_url(), "compare_trees")
  if(is.list(phylo1) & length(phylo1) == 2){
    trees <- phylo1
  } else {
    if(inherits(phylo1, "phylo") & inherits(phylo2, "phylo")){
      trees <- list(phylo1, phylo2)
      # class(trees) <- "multiPhylo"  # this is unnecessary
    }
  }
  if(length(trees)!=2) {
    stop("Must have two trees as input")
  }
  body <- list(tree1_nwk = ape::write.tree(trees[[1]]), tree2_nwk = ape::write.tree(trees[[2]]))
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result$are_same_tree)
}
