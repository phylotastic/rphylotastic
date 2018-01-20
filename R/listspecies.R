#' Insert list of species
#'
#' @param userid A valid gmail address of the user
#' @param listObj A list object
#' @return A list with the id of the new list created
#' @examples
#'   userid = "abusalehmdtayeen@gmail.com"
#'   listObj = list(list_extra_info="", list_description="A sublist on the bird species added",
#'       list_keywords=c("bird", "endangered species", "Everglades"),
#'       list_curator="HD Laughinghouse", list_origin="webapp",
#'       list_curation_date="02-24-2016", list_source="des", list_focal_clade="Aves",
#'       list_title="Bird Species List",list_author=c("Bass", "O. & Cunningham", "R."),
#'       list_date_published="01-01-2017", is_list_public=TRUE,
#'       list_species=list(list(family="",scientific_name="Aix sponsa",
#'           scientific_name_authorship="", vernacular_name="Wood Duck",
#'           phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""),
#'           list(family="",scientific_name="Anas strepera",
#'           scientific_name_authorship="", vernacular_name="Gadwall",
#'           phylum="",nomenclature_code="ICZN",
#'           order="Anseriformes",class="") ))
#'   insert_species_in_list(userid, listObj)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export

insert_species_in_list <- function(userid, listObj) {
  #body <- list(user_id = "abusalehmdtayeen@gmail.com", list = list(list_extra_info="", list_description="A sublist on the bird species added", list_keywords=c("bird", "endangered species", "Everglades"),list_curator="HD Laughinghouse", list_origin="webapp", list_curation_date="02-24-2016", list_source="des", list_focal_clade="Aves", list_title="Bird Species List", list_author=c("Bass", "O. & Cunningham", "R."),  list_date_published="01-01-2017", is_list_public=TRUE, list_species=list(list(family="",scientific_name="Aix sponsa",scientific_name_authorship="", vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""), list(family="",scientific_name="Anas strepera",scientific_name_authorship="", vernacular_name="Gadwall",phylum="",nomenclature_code="ICZN",order="Anseriformes",class="") )))
  url <- paste(get_list_server_url(), 'insert_list', sep="")
  body <- list(user_id = userid, list = listObj)
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result)
}


#' Replace a list of species
#'
#' @param userid A valid gmail address of the user
#' @param access_token Access token of the gmail address
#' @param list_id An integer id of the list to be modified
#' @param speciesObj A species object to replace with
#' @return A list with the old species and new species list
#' @examples
#'   # This gives you the syntax, but since the access token expires after one hour,
#'   # this particular example will not work.
#'   \dontrun{
#'   userid = "abusalehmdtayeen@gmail.com"
#'   access_token = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   list_id = 12
#'   speciesObj = list( list(family="",scientific_name="Aix sponsa",scientific_name_authorship="",
#'   vernacular_name="Wood Duck",phylum="",nomenclature_code="ICZN",order="Anseriformes",class=""))
#'   replace_species_in_list(userid, access_token, list_id, speciesObj)
#'   }
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
replace_species_in_list <- function(userid, access_token, list_id, speciesObj) {
  url <- paste(get_list_server_url(), 'replace_species', sep="")
  body <- list(user_id = userid, access_token = access_token, list_id = list_id, species = speciesObj)
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result)
}


#' Update metadata of a list of species
#'
#' @param userid A valid gmail address of the user
#' @param access_token Access token of the gmail address
#' @param list_id An integer id of the list to be modified
#' @param listObj A list object to update with
#' @return A list with modified list metadata
#' @examples
#'   # This gives you the syntax, but since the access token expires after one hour,
#'   # this particular example will not work.
#'   \dontrun{
#'   userid = "abusalehmdtayeen@gmail.com"
#'   access_token = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   list_id = 12
#'   listObj = list(list_description="A sublist on the bird species",
#'             list_keywords=c("bird","Everglades"))
#'   update_species_in_list(userid, access_token, list_id, listObj)
#' }
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
update_species_in_list <- function(userid, access_token, list_id, listObj) {
  url <- paste(get_list_server_url(), 'update_list', sep="")
  body <- list(user_id = userid, access_token = access_token, list_id = list_id, list = listObj)
  response <- httr::POST(url, body = body, encode = "json")
  result <- httr::content(response,"parsed")

  return(result)
}


#' Get existing list/lists of species
#'
#' @param userid A valid gmail address of the user
#' @param access_token Access token of the gmail address
#' @param list_id An integer id of the list to retrieve
#' @param verbose (optional)By default FALSE and shows minimal meta-data of the list.
#' @param content (optional)By default TRUE and shows the species collection of the list
#' @return An existing list with metadata and content based on parameters
#' @examples
#'   # This gives you the syntax, but since the access token expires after one hour,
#'   # this particular example will not work.
#'   \dontrun{
#'   userid = "abusalehmdtayeen@gmail.com"
#'   access_token = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   list_id = 12
#'   verbose = TRUE
#'   content = FALSE
#'   get_species_from_list(userid, access_token, list_id, verbose, content)
#'   }
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
get_species_from_list <- function(userid, access_token, list_id, verbose=FALSE, content=TRUE) {
  result <- jsonlite::fromJSON(paste(get_list_server_url(), 'get_list?user_id=', userid, '&access_token=', access_token, '&list_id=', list_id, "&verbose=", verbose, "&content=", content, sep=""))
  return(result)
}


#' Remove an existing list of species
#'
#' @param userid A valid gmail address of the user
#' @param access_token Access token of the gmail address
#' @param list_id An integer id of the list to retrieve
#' @return A list with the id of the list removed
#' @examples
#'   # This gives you the syntax, but since the access token expires after one hour,
#'   # this particular example will not work.
#'   \dontrun{
#'   userid = "abusalehmdtayeen@gmail.com"
#'   access_token = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
#'   list_id = 12
#'   remove_species_from_list(userid, access_token, list_id)
#'   }
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
remove_species_from_list <- function(userid, access_token, list_id) {
  result <- jsonlite::fromJSON(paste(get_list_server_url(), 'remove_list?user_id=', userid, '&access_token=', access_token, '&list_id=', list_id, sep=""))
  return(result)
}
