#' Function to pull scientific names from web pages
#'
#' @param URL The URL to extract names from. Can be a pdf url.
#' @param search_engine 1 to use TaxonFinder, 2 to use NetiNeti, 0 to use both
#' @param above_species Boolean. Default to FALSE. If TRUE it will only return scientific names above the species level.
#' @return A vector of scientific names. It returns unique matches.
#' @examples
#' # get scientific names from a wikipedia web page:
#' url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/Plain_pigeon")
#' # get scientific names from a pdf URL:
#' url_get_scientific_names(URL = "http://darwin-online.org.uk/converted/pdf/1897_Insectivorous_F1229.pdf")
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
url_get_scientific_names <- function(URL, search_engine=0, above_species = FALSE){
  results <- jsonlite::fromJSON(paste0(get_base_url(), 'fn/names_url?url=', URL, '&engine=', search_engine))
  results <- unique(results$scientificNames)
  if(above_species){
    results <- taxa_get_above_species(results)
  }
  return(results)
}

#' Function to pull scientific names from text
#'
#' This takes a string of text and extracts any scientific names in the text. Other words in the text are ignored.
#'
#' @param text The text string to extract names from
#' @inheritParams url_get_scientific_names
#' @return A vector of scientific names
#' @examples
#' text <- "Formica polyctena is a species of European red wood ant in
#'    the genus Formica. The pavement ant, Tetramorium caespitum
#'    is an ant native to Europe."
#' print(text_get_scientific_names(text))
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
text_get_scientific_names <- function(text, search_engine=0, above_species = FALSE) {
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
#' @inheritParams url_get_scientific_names
#' @return A vector of scientific names
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
#' @export
file_get_scientific_names <- function(file_name, search_engine=0, above_species = FALSE) {
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
