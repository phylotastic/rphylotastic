test_that("Get species works", {
  results <- GetSpeciesFromTaxon("vulpes")
  expect_gte(length(results), 12)
})


test_that("Separating dark species works for OToL", {
  results <- SeparateDarkTaxaOToL("vulpes")
  expect_gte(length(results$dark), 1)
  expect_gte(length(results$known), 5)
  expect_gte(length(results$fraction.dark), 0)
  expect_lte(length(results$fraction.dark), 1)
})


test_that("Separating dark species works for NCBI", {
  results <- SeparateDarkTaxaOToL("Poa")
  expect_gte(length(results$dark), 5)
  expect_gte(length(results$known), 5)
  expect_gte(length(results$fraction.dark), 0)
  expect_lte(length(results$fraction.dark), 1)
})
