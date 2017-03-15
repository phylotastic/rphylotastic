test_that("Loading names from a standard web page works correctly", {
  results <- GetScientificNamesFromURL("http://en.wikipedia.org/wiki/Ant")
  expect_gte(length(results), 50)
  expect_true( any(grepl("Formicidae", results)))
})

test_that("Loading names from an online PDF works correctly", {
  results <- GetScientificNamesFromURL("https://www.fws.gov/westvirginiafieldoffice/PDF/beechridgehcp/Appendix_D_Table_D-1.pdf")
  expect_gte(length(results), 50)
  expect_true( any(grepl("Micropterus", results)))
})


test_that("Loading names from a text string works correctly", {
  results <- GetScientificNamesFromText("Formica polyctena is a species of European red wood ant in the genus Formica. The pavement ant, Tetramorium caespitum is an ant native to Europe. Pseudomyrmex is a genus of stinging, wasp-like ants. Adetomyrma venatrix is an endangered species of ants endemic to Madagascar. Carebara diversa is a species of ants in the subfamily Formicinae. It is found in many Asian countries.")
  expect_gte(length(results), 2)
  expect_true( any(grepl("Formica", results)))
})
