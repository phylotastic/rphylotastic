test_that("Resolving names with GNR works", {
  taxa <- c("Fake Species", "Setophaga striata", "Setophaga megnolia", "Another fake species", "Setophaga angilae", "Setophaga plumbea", "Setophaga virens")
  expect_warning(taxa.new <- ResolveNamesWithGNR(taxa))
  expect_gte(length(taxa.new), 3)
})
