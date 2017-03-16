test_that("Loading a tree from OToL", {
  taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae", "Formica polyctena", "Tetramorium caespitum", "Pseudomyrmex", "Carebara diversa", "Formicinae")
  tree <- GetOToLTree(taxa)
  expect_equal(class(tree), "phylo")
  expect_gte(ape::Ntip(tree), 5)
})
