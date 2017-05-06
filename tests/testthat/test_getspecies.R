test_that("Get species works", {
  results <- GetSpeciesFromTaxon("vulpes")
  expect_gte(length(results), 12)
})


test_that("Separating dark species works", {
  results <- SeparateDarkTaxa("vulpes")
  expect_gte(length(results), 12)
})
