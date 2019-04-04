test_that("Get tree from data works", {
  my_data <- data.frame(body_size = runif(10), brain_size= runif(10)) # made up
  rownames(my_data) <- c("Strigops habroptilus", "Struthio camelus", "Dromaius novaehollandiae", "Tyto alba", "Nestor notabilis", "Bubo virginianus", "Haliaeetus leucocephalus", "Pandion haliaetus", "Tyto soumagnei", "Xenoglaux loweryi")
  result_otol_otol <- data_get_tree(my_data)
  expect_equal(ape::Ntip(result_otol_otol$phy),10)
  expect_equal(nrow(result_otol_otol$data),10)
  result_otol_datelife<- data_get_tree(my_data, tree_source="datelife")
  expect_true(ape::is.ultrametric(result_otol_datelife$phy))
  result_gnr_datelife <- data_get_tree(my_data, tnrs_source="gnr", tree_source="datelife")
  expect_true(ape::is.ultrametric(result_gnr_datelife$phy))
})


test_that("Get tree from data works if someone puts in a vector", {
  my_vector <- runif(10) # made up
  names(my_vector) <- c("Strigops habroptilus", "Struthio camelus", "Dromaius novaehollandiae", "Tyto alba", "Nestor notabilis", "Bubo virginianus", "Haliaeetus leucocephalus", "Pandion haliaetus", "Tyto soumagnei", "Xenoglaux loweryi")
  expect_warning(result_otol_otol <- data_get_tree(my_vector))
  expect_equal(ape::Ntip(result_otol_otol$phy),10)
})
