#' Function to get tree for a traits set.
#'
#' A common use case is having a set of traits for a set of species and wanting to
#' get a tree for those species. This involves resolving to the same taxonomy,
#' getting a tree for those species, and then (optionally) pruning the traits and
#' tree to the same set of taxa (this is optional: there are approaches to
#' make up traits or phylogenetic placement in the absence of information).
#'
#' For sources of trees, besides the standard ones (Open Tree of Life, Phylomatic),
#' datelife is also an option. For this, you will have had to install the
#' datelife package (which is only suggested, not required, by this code).
#' It will return the chronogram in the tree store with the most overlap to
#' your set of taxa by default, but you can change options with summary_format;
#' see ?datelife::datelife_search for more info
#'
#' @param traits Data.frame with species names as rownames
#' @param tnrs_source Source for taxonomic name resolution; options are "otol" and "gnr". If set to NULL, assumes names are fine as is
#' @param tree_source Source for tree; options are "otol", "phylomatic", "datelife".
#' @param prune If TRUE, delete taxa to matching sets only
#' @param summary_format What format to return from datelife
#' @param ... Other options to pass to datelife::datelife_search
#' @importFrom datelife opentree_chronograms
#' @return list with a phy and a traits object, both pruned to the same taxon set,
#' as well as citation information for the sources of the taxonomic resolution
#' and phylogeny (also cite this package and, if you use it, datelife)
#' @export
traits_get_tree <- function(traits, tnrs_source="otol", tree_source="otol", prune=TRUE, summary_format="phylo_biggest", ...) {
    traits <- my_vector
  if(inherits(traits, "numeric")) {
    traits <- data.frame(traits, stringsAsFactors=FALSE)
    # rownames(traits2) <- names(traits)
    # traits <- traits2
    warning("This function takes in data.frames, not vectors. Tried to convert traits to data.frame")
  }
  if(nchar(rownames(traits)[1])<2) {
    stop("This expects rownames of traits to be taxon names")
  }
  citation <- "Taxonomic name resolution done with OpenTree: Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy into a comprehensive tree of life. Proceedings of the National Academy of Sciences 112.41 (2015): 12764-12769.\n\n"
  resolver_fn <- taxa_resolve_names_with_otol
  if(!is.null(tnrs_source)) {
    if(tnrs_source=="gnr") {
      resolver_fn <- taxa_resolve_names_with_gnr
      citation <- "Taxonomic name resolution done with Global Names: See http://globalnames.org/bibliography/ for papers.\n\n"

    }
    resolved.names <- sapply(rownames(traits),resolver_fn)
    traits <- traits[!sapply(resolved.names, is.null),, drop = FALSE] #prune TNRS failures
    rownames(traits) <- resolved.names[!sapply(resolved.names, is.null)]
  }
  tree_fun <- taxa_get_otol_tree
  tree_citation <- "Phylogeny from a synthesis from OpenTree: Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy into a comprehensive tree of life. Proceedings of the National Academy of Sciences 112.41 (2015): 12764-12769."
  if(tree_source=="phylomatic") {
    tree_fun <- taxa_get_phylomatic_tree
    tree_citation <- "Phylogeny from a synthesis from Phylomatic: Webb, C.O., and M.J. Donoghue (2005) Phylomatic: tree assembly for applied phylogenetics. Molecular Ecology Notes 5: 181-183."
  }

  phy <- NULL
  if(tree_source=="datelife") {
    # data(opentree_chronograms, package="datelife")
    phy <- datelife::datelife_search(rownames(traits), summary_format=summary_format, ...)
    if(summary_format=="phylo_biggest") {
      tree_citation <- paste("Phylogeny from ", phy$citation, "\n\n", sep="")
    } else {
      tree_citation <- paste("Phylogeny information from the following sources:\n", paste(datelife::datelife_search(rownames(traits), summary_format="citations", ...), collapse="\n"))
    }
  } else {
    phy <- tree_fun(rownames(traits))
  }
  if(prune) {
    traits <- traits[!(rownames(traits) %in% phy$tip.label),]
  }
  return(list(traits=traits, phy=phy, citation=paste(citation, tree_citation)))
}
