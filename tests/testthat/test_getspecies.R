test_that("Get species works", {
  results <- taxon_get_species("Vulpes")
  expect_gte(length(results), 10)
})

test_that("Get country species works", {
  results <- taxon_get_species_and_country("Felidae", "Nepal")
  expect_gte(length(results), 10)
  expect_equal(grepl("Prionailurus viverrinus", results), c(FALSE, FALSE, TRUE,FALSE, FALSE,FALSE, FALSE,FALSE, FALSE,FALSE, FALSE))
})

test_that("Get genome species works", {
  results <- taxon_get_species_with_genome("Rodentia")
  expect_gte(length(results), 30)
  expect_equal(grep("Mus", results, value=TRUE), c("Mus musculus molossinus","Mus musculus musculus","Mus spretus", "Mus pahari", "Mus musculus domesticus", "Mus musculus castaneus", "Mus musculus", "Mus caroli"))
})

test_that("Separating dark species works for OToL", {
  results <- taxon_separate_dark_taxa_using_otol("vulpes")
  expect_gte(length(results$dark), 1)
  expect_gte(length(results$known), 5)
  expect_gte(length(results$fraction.dark), 0)
  expect_lte(length(results$fraction.dark), 1)
})


test_that("Separating dark species works for NCBI", {
  results <- taxon_separate_dark_taxa_using_otol("Poa")
  expect_gte(length(results$dark), 5)
  expect_gte(length(results$known), 5)
  expect_gte(length(results$fraction.dark), 0)
  expect_lte(length(results$fraction.dark), 1)
})
