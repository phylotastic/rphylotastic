#' Function to get tree for a data set.
#'
#' A common use case is having a set of data for a set of species and wanting to
#' get a tree for those species. This involves resolving to the same taxonomy,
#' getting a tree for those species, and then (optionally) pruning the data and
#' tree to the same set of taxa (this is optional: there are approaches to
#' make up trait data or phylogenetic placement in the absence of information).
#'
#' For sources of trees, besides the standard ones (Open Tree of Life, Phylomatic),
#' datelife is also an option. For this, you will have had to install the
#' datelife package (which is only suggested, not required, by this code).
#' It will return the chronogram in the tree store with the most overlap to
#' your set of taxa by default, but you can change options with summary_format;
#' see ?datelife::datelife_search for more info
#'
#' @param data Data.frame with species names as rownames
#' @param tnrs_source Source for taxonomic name resolution; options are "otol" and "gnr". If set to NULL, assumes names are fine as is
#' @param tree_source Source for tree; options are "otol", "phylomatic", "datelife".
#' @param prune If TRUE, delete taxa to matching sets only
#' @param summary_format What format to return from datelife
#' @param ... Other options to pass to datelife::datelife_search
#' @return list with a phy and a data object, both pruned to the same taxon set,
#' as well as citation information for the sources of the taxonomic resolution
#' and phylogeny (also cite this package and, if you use it, datelife)
#' @export
data_get_tree <- function(data, tnrs_source="otol", tree_source="otol", prune=TRUE, summary_format="phylo_biggest", ...) {
  if(is.null(dim(data))) {
    warning("This function takes in data.frames, not vectors. Trying to convert to data.frame")
    data2 <- data.frame(trait=data, stringsAsFactors=FALSE)
    rownames(data2) <- names(data)
    data <- data2
  }
  if(nchar(rownames(data)[1])<2) {
    stop("This expects rownames of data to be taxon names")
  }
  citation <- "Taxonomic name resolution done with OpenTree: Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy into a comprehensive tree of life. Proceedings of the National Academy of Sciences 112.41 (2015): 12764-12769.\n\n"
  resolver_fn <- taxa_resolve_names_with_otol
  if(!is.null(tnrs_source)) {
    if(tnrs_source=="gnr") {
      resolver_fn <- taxa_resolve_names_with_gnr
      citation <- "Taxonomic name resolution done with Global Names: See http://globalnames.org/bibliography/ for papers.\n\n"

    }
    resolved.names <- sapply(rownames(data),resolver_fn)
    data <- data[!sapply(resolved.names, is.null),] #prune TNRS failures
    rownames(data) <- resolved.names[!sapply(resolved.names, is.null)]
  }
  tree_fun <- taxa_get_otol_tree
  tree_citation <- "Phylogeny from a synthesis from OpenTree: Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy into a comprehensive tree of life. Proceedings of the National Academy of Sciences 112.41 (2015): 12764-12769."
  if(tree_source=="phylomatic") {
    tree_fun <- taxa_get_phylomatic_tree
    tree_citation <- "Phylogeny from a synthesis from Phylomatic: Webb, C.O., and M.J. Donoghue (2005) Phylomatic: tree assembly for applied phylogenetics. Molecular Ecology Notes 5: 181-183."
  }

  phy <- NULL
  if(tree_source=="datelife") {
    data(opentree_chronograms, package="datelife")
    phy <- datelife::datelife_search(rownames(data), summary_format=summary_format, ...)
    if(summary_format=="phylo_biggest") {
      tree_citation <- paste("Phylogeny from ", phy$citation, "\n\n", sep="")
    } else {
      tree_citation <- paste("Phylogeny information from the following sources:\n", paste(datelife::datelife_search(rownames(data), summary_format="citations", ...), collapse="\n"))
    }
  } else {
    phy <- tree_fun(rownames(data))
  }
  if(prune) {
    data <- data[!(rownames(data) %in% phy$tip.label),]
  }
  return(list(data=data, phy=phy, citation=paste(citation, tree_citation)))
}
