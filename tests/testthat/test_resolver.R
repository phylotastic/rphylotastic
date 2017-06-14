test_that("Resolving names with GNR works", {
  taxa <- c("Fake Species", "Setophaga striata", "Setophaga megnolia", "Another fake species", "Setophaga angilae", "Setophaga plumbea", "Setophaga virens")
  expect_gte(length(taxa.new <- ResolveNamesWithGNR(taxa)), 5)
  taxa <- c("Fake Species", "Another fake species")
  expect_warning(taxa.new <- ResolveNamesWithGNR(taxa))
})
