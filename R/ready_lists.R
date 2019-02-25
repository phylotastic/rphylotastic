#' Flowering plants families from Open Tree Taxonomy
#'
#' @name flower_plant_fams
#' @docType data
#' @author Luna L. Sanchez-Reyes \email{lsanche7@utk.edu}
#' Brian O'Meara \email{bomeara@utk.edu}
#' @source \url{https://tree.opentreeoflife.org/about/taxonomy-version/ott3.0}
#' @format A character vector
#' @keywords otol magnoliophyta
#' @details
#' flower_plant_fams <- datelife::get_ott_children(ott_ids = 99252, ott_rank = "family")
#' flower_plant_fams <- flower_plant_fams[[1]]
#' flower_plant_fams <- rownames(flower_plant_fams)[as.character(flower_plant_fams[,"rank"]) == "family"]
#' usethis::use_data(flower_plant_fams, overwrite = TRUE)
"flower_plant_fams"

#' Flowering plants families from Open Tree Taxonomy
#'
#' @name terrestrial_plant_orders
#' @docType data
#' @author Luna L. Sanchez-Reyes \email{lsanche7@utk.edu}
#' Brian O'Meara \email{bomeara@utk.edu}
#' @source \url{https://tree.opentreeoflife.org/about/taxonomy-version/ott3.0}
#' @format A character vector
#' @keywords otol magnoliophyta
#' @details
#' terrestrial_plant_orders <- datelife::get_ott_children(ott_ids = 99252, ott_rank = "order")
#' terrestrial_plant_orders <- terrestrial_plant_orders[[1]]
#' terrestrial_plant_orders <- rownames(terrestrial_plant_orders)[as.character(terrestrial_plant_orders[,"rank"]) == "order"]
#' usethis::use_data(flower_plant_fams, overwrite = TRUE)
"terrestrial_plant_orders"
