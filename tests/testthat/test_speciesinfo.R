test_that("Get species info from EOL works", {
  results <- species_get_info(c('Myrmecocystus mexicanus'))
  expect_equal(results[1,2:3]$eol_id, 461196)
  expect_equal(results[1,1], "https://eol.org/pages/461196")
})

test_that("Get image metadata of species works", {
  results <- species_get_image_data(c('Myrmecocystus mexicanus'))
  #results[1,3] is total_images
  expect_lte(results[1,3], 5)
  # the following two lines are a bad test because they can change through time in eol...
  expect_equal(results[1,1][[1]]$eolMediaURL[[1]], "https://content.eol.org/data/media/72/66/fa/537.11636218.jpg")
  #result[1,1][[1]] is a dataframe
  expect_equal(results[1,1][[1]]$eolThumbnailURL[[1]], "https://content.eol.org/data/media/72/66/fa/537.11636218.98x68.jpg")
})
