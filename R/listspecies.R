#' Insert list of species
#'
#' @param userid A valid gmail address of the user
#' @param listObj A list object
#' @return A list with the id of the new list created
#' @examples
#'   userid = "abusalehmdtayeen@gmail.com"
#'   listObj = list(list_extra_info="", list_description="A sublist on the bird species added", list_keywords=c("bird", "endangered species", "Everglades"),list_curator="HD Laughinghouse", list_origin="webapp", list_curation_date="02-24-2016", list_source="des", list_focal_clade="Aves", list_title="Bird Species List", list_author=c("Bass", "O. & Cunningham", "R."),  list_date_published="01-01-2017", is_list_public=TRUE, list_species=list(list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""), list(family="",scientific_name="Anas strepera",scientific_name_authorship="", vernacular_name="Gadwall",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") ))
#'   InsertListSpecies(userid, listObj)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
#library(httr)
InsertListSpecies <- function(userid, listobj) {
  #body <- list(user_id = "abusalehmdtayeen@gmail.com", list = list(list_extra_info="", list_description="A sublist on the bird species added", list_keywords=c("bird", "endangered species", "Everglades"),list_curator="HD Laughinghouse", list_origin="webapp", list_curation_date="02-24-2016", list_source="des", list_focal_clade="Aves", list_title="Bird Species List", list_author=c("Bass", "O. & Cunningham", "R."),  list_date_published="01-01-2017", is_list_public=TRUE, list_species=list(list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""), list(family="",scientific_name="Anas strepera",scientific_name_authorship="", vernacular_name="Gadwall",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") )))	
  url <- paste(GetListServerURL(), 'insert_list', sep="")
  body <- list(user_id = userid, list = listobj)
  response <- POST(url, body = body, encode = "json")
  result <- content(response,"parsed")	
  
  return(result)
}


#' Replace a list of species
#'
#' @param userid A valid gmail address of the user
#' @param accesstoken A valid gmail address of the user
#' @param listid An integer id of the list to be modified
#' @param speciesObj A species object to replace with  
#' @return A list with the old species and new species list
#' @examples
#'   userid = "abusalehmdtayeen@gmail.com"
#'   accesstoken = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   listid = 12
#'   speciesObj = list( list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") )
#'   ReplaceListSpecies(userid, accesstoken, listid, speciesObj)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
ReplaceListSpecies <- function(userid, accesstoken, listid, speciesobj) {
  url <- paste(GetListServerURL(), 'replace_species', sep="")
  body <- list(user_id = userid, access_token = accesstoken, list_id = listid, species = speciesobj)
  response <- POST(url, body = body, encode = "json")
  result <- content(response,"parsed")	
  
  return(result)
}


#' Update metadata of a list of species
#'
#' @param userid A valid gmail address of the user
#' @param accesstoken A valid gmail address of the user
#' @param listid An integer id of the list to be modified
#' @param listObj A species object to replace with  
#' @return A list with modified list metadata
#' @examples
#'   userid = "abusalehmdtayeen@gmail.com"
#'   accesstoken = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   listid = 12
#'   listObj = list(list_description="A sublist on the bird species", list_keywords=c("bird","Everglades"))
#'   UpdateListSpecies(userid, accesstoken, listid, listObj)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
UpdateListSpecies <- function(userid, accesstoken, listid, listobj) {
  url <- paste(GetListServerURL(), 'update_list', sep="")
  body <- list(user_id = userid, access_token = accesstoken, list_id = listid, list = listobj)
  response <- POST(url, body = body, encode = "json")
  result <- content(response,"parsed")	
  
  return(result)
}


#' Get existing list/lists of species
#'
#' @param userid A valid gmail address of the user
#' @param accesstoken A valid gmail address of the user
#' @param listid An integer id of the list to be modified
#' @param verbose A species object to replace with 
#' @param content A species object to replace with  
#' @return A list with modified list metadata
#' @examples
#'   userid = "abusalehmdtayeen@gmail.com"
#'   accesstoken = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   listid = 12
#'   listObj = list(list_description="A sublist on the bird species", list_keywords=c("bird","Everglades"))
#'   UpdateListSpecies(userid, accesstoken, listid, listObj)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/blob/master/ServiceDescription/PhyloServicesDescription.md}
#' @export
UpdateListSpecies <- function(userid, accesstoken, listid, listobj) {
  url <- paste(GetListServerURL(), 'update_list', sep="")
  body <- list(user_id = userid, access_token = accesstoken, list_id = listid, list = listobj)
  response <- POST(url, body = body, encode = "json")
  result <- content(response,"parsed")	
  
  return(result)
}

