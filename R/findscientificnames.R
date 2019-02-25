#' Function to pull scientific names from web pages
#'
#' @param URL The URL to extract names from
#' @param search_engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @examples
#' URL <- "https://en.wikipedia.org/wiki/Plain_pigeon"
#' print(url_get_scientific_names(URL))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
url_get_scientific_names <- function(URL, search_engine=0) {
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'fn/names_url?url=', URL, '&engine=', search_engine))
  return(results$scientificNames)
}

#' Function to pull scientific names from text
#'
#' This takes a string of text and extracts any scientific names in the text. Other words in the text are ignored.
#'
#' @param text The text string to extract names from
#' @param search_engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @examples
#' text <- "Formica polyctena is a species of European red wood ant in
#'    the genus Formica. The pavement ant, Tetramorium caespitum
#'    is an ant native to Europe."
#' print(text_get_scientific_names(text))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
text_get_scientific_names <- function(text, search_engine=0) {
  #results <- jsonlite::fromJSON(utils::URLencode(paste(get_base_url(), 'fn/names_url?url=', text, '&engine=', search_engine, sep="")))
  results <- jsonlite::fromJSON((paste0(get_base_url(), 'fn/names_text?text=', utils::URLencode(gsub("[^[:alnum:] ]", "",as.character(text))), '&engine=', search_engine)))
  return(results$scientificNames)
}

#' Function to pull scientific names from file
#'
#' This uploads a file (a PDF, Microsoft Word document, plain text file, etc.) and extracts all scientific names from it. For example, you can input a PDF of a scientific article and it will return all the scientific names in that article.
#'
#' It requires that curl is installed on your system.
#'
#' @param file_name The file path and name to extract names from
#' @param search_engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @return A vector of scientific names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
file_get_scientific_names <- function(file_name, search_engine=0) {
  # results <- jsonlite::fromJSON(httr::POST(paste0(get_base_url(), 'fn/names_file'), body=list(inputFile=httr::upload_file(file_name), engine=search_engine)))  #not working -- returns 200 and valid json, but no names
  original_dir <- getwd()
  newdir <- dirname(file_name)
  setwd(newdir)
  file_name_only <- basename(file_name)
  # results <- jsonlite::fromJSON(system(paste0("curl -X POST http://phylo.cs.nmsu.edu:5004/phylotastic_ws/fn/names_file -F 'inputFile=@", file_name_only, "'  -F 'engine=", search_engine, "'"), intern=TRUE))
  results <- jsonlite::fromJSON(system(paste0("curl -X POST ", get_base_url(), "/fn/names_file -F 'inputFile=@", file_name_only, "'  -F 'engine=", search_engine, "'"), intern=TRUE))
  setwd(original_dir)
  return(results$scientificNames)
}
