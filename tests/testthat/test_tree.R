test_that("Loading a tree from OToL", {
  taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae", "Formica polyctena", "Tetramorium caespitum", "Pseudomyrmex", "Carebara diversa", "Formicinae")
  tree <- GetOToLTree(taxa)
  expect_equal(class(tree), "phylo")
  expect_gte(ape::Ntip(tree), 5)
})

test_that("Loading a tree from Phylomatic", {
  taxa <- c("Panthera leo","Panthera onca","Panthera tigris","Panthera uncia")
  tree_nwk <- GetPhylomaticTree(taxa)
  expect_equal(tree_nwk, "((Panthera_leo:6.3,Panthera_onca:6.3):0.1,Panthera_tigris:6.4):159.8;")
})


test_that("Loading a tree from NCBI taxonomy", {
  taxa <- c("Setophaga striata","Setophaga magnolia","Setophaga angelae","Setophaga plumbea","Setophaga virens")
  tree_nwk <- GetTaxonomicTree(taxa)
  expect_equal(tree_nwk, "(Setophaga_virens,Setophaga_angelae,Setophaga_plumbea,Setophaga_striata,Setophaga_magnolia);")
})
