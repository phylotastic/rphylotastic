% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listspecies.R
\name{update_species_in_list}
\alias{update_species_in_list}
\title{Update metadata of a list of species}
\usage{
update_species_in_list(userid, access_token, list_id, listObj)
}
\arguments{
\item{userid}{A valid gmail address of the user}

\item{access_token}{Access token of the gmail address}

\item{list_id}{An integer id of the list to be modified}

\item{listObj}{A list object to update with}
}
\value{
A list with modified list metadata
}
\description{
Update metadata of a list of species
}
\examples{
  # This gives you the syntax, but since the access token expires after one hour,
  # this particular example will not work.
  \dontrun{
  userid = "abusalehmdtayeen@gmail.com"
  access_token = "ya29..zQLmLjbyujJjwV6RVSM2sy-mkeaKu-9"
  list_id = 12
  listObj = list(list_description="A sublist on the bird species",
            list_keywords=c("bird","Everglades"))
  update_species_in_list(userid, access_token, list_id, listObj)
}
}
\seealso{
\url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription}
}
