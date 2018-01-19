#test_that("Inserting a list of species", {
#  userid <- "abusalehmdtayeen@gmail.com"
#  listObj <- list(list_extra_info="", list_description="A sublist on the bird species added", list_keywords=c("bird", "endangered species", "Everglades"),list_curator="HD Laughinghouse", list_origin="webapp", list_curation_date="02-24-2016", list_source="des", list_focal_clade="Aves", list_title="Bird Species List", list_author=c("Bass", "O. & Cunningham", "R."),  list_date_published="01-01-2017", is_list_public=TRUE, list_species=list(list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""), list(family="",scientific_name="Anas strepera",scientific_name_authorship="", vernacular_name="Gadwall",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") ))
#  result <- insert_species_in_list(userid, listObj)
#  list_id <- result$list_id
#  msg<-result$message
#  expect_equal(msg, "Success")
#})


#test_that("Replacing a list of species", {
#  userid <- "abusalehmdtayeen@gmail.com"
#  access_token <- "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#  list_id <- 12
#  speciesObj <- list( list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") )
#  result <- replace_species_in_list(userid, access_token, list_id, speciesObj)
#  msg<-result$message
#  expect_equal(msg, "Success")
#})

	
#test_that("Updating metadata of a list of species", {
#  userid <- "abusalehmdtayeen@gmail.com"
#  access_token <- "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#  list_id <- 12
#  listObj <- list(list_description="A sublist on the bird species", list_keywords=c("bird","Everglades"))
#  result <- update_species_in_list(userid, access_token, list_id, listObj)
#  msg <-result$message
#  expect_equal(msg, "Success")
#})
