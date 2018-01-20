test_that("Resolving names with GNR works", {
  taxa <- c("Fake Species", "Setophaga striata", "Setophaga megnolia", "Another fake species", "Setophaga angilae", "Setophaga plumbea", "Setophaga virens")
  expect_gte(length(taxa.new <- taxa_resolve_names_with_gnr(taxa)), 3)
  taxa <- c("Fake Species", "Another fake species")
  expect_warning(taxa.new <- taxa_resolve_names_with_gnr(taxa))
})
