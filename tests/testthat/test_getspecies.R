test_that("Get species works", {
  results <- GetSpeciesFromTaxon("vulpes")
  expect_gte(length(results), 12)
})
