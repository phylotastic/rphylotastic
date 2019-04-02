# render_pdf <- function(reportname, dir, placeholder) {
#   original.dir <- getwd()
#   print(original.dir)
#   # setwd(dir)
#   system(paste0('pandoc ', paste0(reportname, '.md'), ' -o ', paste0(reportname, '.pdf --pdf-engine=xelatex -V mainfonts="DejaVu Sans"')))
#   # setwd(original.dir)
#   # pandoc -o emoji.pdf --pdf-engine=lualatex -V mainfonts="DejaVu Sans"
#   # pandoc -o emoji.pdf --pdf-engine=xelatex  -V mainfonts="DejaVu Sans"
# }
make_allservices <- function(all_categories){
    all_services <- vector(mode = "list", length = length(all_categories))
    names(all_services) <- all_categories
    all_services[[all_categories[1]]] <- c("NCBI_common_name",
        "EBI_common_name",
        "ITIS_common_name",
        "TROPICOS_common_name",
        "EOL_common_name")
    all_services[[all_categories[2]]] <- c("GNRD_wrapper_URL",
        "GNRD_wrapper_text;",
        "GNRD_wrapper_file",
        "TaxonFinder_wrapper_URL;",
        "TaxonFinder_wrapper_text")
    all_services[[all_categories[3]]] <- c("OToL_TNRS_wrapper",
        "GNR_TNRS_wrapper",
        "iPlant_TNRS_wrapper")
    all_services[[all_categories[4]]] <- c("Taxon_all_species",
        "Taxon_country_species",
        "Taxon_genome_species",
        "Taxon_popular_species")
    all_services[[all_categories[5]]] <- c("Image_url_species",
        "Info_url_species",
        "ECOS_Conservation")
    all_services[[all_categories[6]]] <- c("OToL_wrapper_Tree;",
        "OToL_supported_studies",
        "Phylomatic_wrapper_Tree",
        "Treebase_Tree",
        "Supersmart_wrapper_Tree")
    all_services[[all_categories[7]]] <- c("Datelife_scale_tree",
        "OToL_scale_tree")
    all_services[[all_categories[8]]] <- c("Compare_trees")
    all_services[[all_categories[9]]] <- c("Add_new_list; Get_list;",
        "Replace_species_list;",
        "Update_metadata_list;",
        "Remove_list;")
    # concatenate the category to include the general description:
    all_services <- mapply(c, all_categories, all_services)
    # all_services[[all_categories[]]] <- c("")
    return(all_services)
}


make_alldescriptions <- function(all_services){
    all_descriptions <- vector(mode = "list", length(all_services))
    names(all_descriptions) <- all_categories <- names(all_services)
    all_descriptions[[all_categories[1]]] <- c("Get the scientific name of a species from its common name (vernacular name) using",
      "NCBI data base",
      "EBI",
      "ITIS",
      "TROPICOS",
      "EOL")
    all_descriptions[[all_categories[2]]] <- c("Scrape scientific names from a URL, text or any tipe of file",
      "using Global Names Recognition and Discovery (GNRD) services", "", "",
      "using Taxon Finder", "")
    all_descriptions[[all_categories[3]]] <- c(
      "Match input scientific names to formal taxonomic names and resolve mismatches",
      "using the Open Tree of Life tool taxonomy",
      "using the Global Names Resolver tool at http://resolver.globalnames.org which includes several taxonomies",
      "using iPlant collaborative services from https://tnrs.iplantcollaborative.org"
    )
    all_descriptions[[all_categories[4]]] <- c("Get scientific names of species that belong to a given higher taxon name",
      "",
      "and are found in a given country following iNaturalist data base",
      "and have a genome sequence submitted in NCBI",
      "and match the most popular species within the taxon using OneZoom tool")
    all_descriptions[[all_categories[5]]] <- c("Get various information of a species such as",
      "image urls and corresponding license information using EOL",
      "information urls from EOL",
      "conservation status from ECO services")
    all_descriptions[[all_categories[6]]] <- c("Get phylogenetic trees from a list of taxa",
      "from OToL and the supporting studies", "", "from Phylomatic", "from TreeBase", "using supersmart")
    all_descriptions[[all_categories[7]]] <- c("Scale branch lengths of a tree relative to time",
      "using the DateLife service",
      "using OToLs unoficial scaling service")
    all_descriptions[[all_categories[8]]] <- c("Compare two phylogenetic trees symmetrically", "")
    all_descriptions[[all_categories[9]]] <- c("Save, publish, access, remove or update lists of names.", rep("", 4))
    return(all_descriptions)
}

make_table1 <- function(all_services, all_descriptions){
    table1 <- data.frame(Web_Service = unlist(all_services), Description = all_descriptions)
    rowsiesfoo <- function(x){
      res <- length(x[[1]])
      for(i in 2:length(x)){
        res <- c(res, res[i-1] + length(x[[i]]))
      }
      res
    }
    # get rows that will have an indent
    rowsies <- rowsiesfoo(all_services)
    rowsies2 <- 1:max(rowsies)
    remove <- rowsies-sapply(all_services, length)+1
    rowsies2 <- rowsies2[-remove]
    print(knitr::kable(table1, caption = "", row.names = FALSE, format = "latex", booktabs = T, linesep = "")
        %>% kableExtra::kable_styling(latex_options = "scale_down")
        %>% kableExtra::add_indent(rowsies2)
        %>% column_spec(1, width = "7cm")
        %>% column_spec(2, width = "10cm")
        %>% as_image(file = "table1.png") #, width = 6
    )
    # return(table1)
}
