test_that("Get species info works", {
  results <- species_get_info(c('Cervus elaphus'))
  expect_equal(results[1,2:3]$eol_id, 328649)
  expect_equal(results[1,1], "http://eol.org/328649?action=overview&controller=taxa")
})

test_that("Get image metadata of species works", {
  results <- GetImageDataSpecies(c('Cervus elaphus'))
  #results[1,3] is total_images
  expect_lte(results[1,3], 5)
  expect_equal(results[1,1][[1]]$eolMediaURL[[1]], "http://media.eol.org/content/2015/05/20/11/96953_orig.jpg")
  #result[1,1][[1]] is a dataframe	
  expect_equal(results[1,1][[1]]$eolThumbnailURL[[1]], "http://media.eol.org/content/2015/05/20/11/96953_98_68.jpg")	
})
