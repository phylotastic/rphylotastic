test_that("Resolving names with GNR works", {
  taxa <- c("Setophaga striata", "Setophaga megnolia", "Setophaga angilae", "Setophaga plumbea", "Setophaga virens")
  expect_warning(taxa.new <- ResolveNamesWithGNR(taxa))
  expect_gte(length(taxa.new), 3)
})
