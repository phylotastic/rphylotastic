test_that("Loading a tree from OToL", {
  taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae", "Formica polyctena", "Tetramorium caespitum", "Pseudomyrmex", "Carebara diversa", "Formicinae")
  tree <- taxa_get_otol_tree(taxa)
  expect_equal(class(tree), "phylo")
  expect_gte(ape::Ntip(tree), 5)
})

test_that("Loading a tree from Phylomatic", {
  taxa <- c("Panthera leo","Panthera onca","Panthera tigris","Panthera uncia")
  tree <- taxa_get_phylomatic_tree(taxa)
  expect_equal("phylo", class(tree))
  tree2 <- taxa_get_phylomatic_tree(taxa = c("elephas maximus", "felis silvestris", "homo sapiens", "delphinus delphus"))
  expect_true(any(grepl("Homo", tree2$tip.label)))
  tree3 <- taxa_get_phylomatic_tree(taxa = c("elephas", "felis", "homo", "delphinus"))
  # for some reason when using genus names and not species names, tip.label names are empty and the following is failing
  skip_on_cran()
  skip_on_travis()
  skip(message = "still need to fix dropped names for higher taxa in taxa_get_phylomatic_tree")
  expect_true(any(grepl("Homo", tree3$tip.label)))
})


# test_that("Loading a tree from NCBI taxonomy", {
#   taxa <- c("Setophaga striata","Setophaga magnolia","Setophaga angelae","Setophaga plumbea","Setophaga virens")
#   tree <- taxa_get_taxonomic_tree(taxa)
#   expect_equal("phylo", class(tree))
# })
