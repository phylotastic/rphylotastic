test_that("Get species info works", {
  results <- species_get_info(c('Myrmecocystus mexicanus'))
  expect_equal(results[1,2:3]$eol_id, 461196)
  expect_equal(results[1,1], "http://eol.org/461196?action=overview&controller=taxa")
})

test_that("Get image metadata of species works", {
  results <- species_get_image_data(c('Myrmecocystus mexicanus'))
  #results[1,3] is total_images
  expect_lte(results[1,3], 5)
  expect_equal(results[1,1][[1]]$eolMediaURL[[1]], "http://media.eol.org/content/2014/11/22/12/30536_orig.jpg")
  #result[1,1][[1]] is a dataframe
  expect_equal(results[1,1][[1]]$eolThumbnailURL[[1]], "http://media.eol.org/content/2014/11/22/12/30536_98_68.jpg")
})
