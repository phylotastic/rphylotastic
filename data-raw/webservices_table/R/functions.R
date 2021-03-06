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
    all_services[[all_categories[2]]] <- c("GNRD_wrapper_URL;",
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
    all_descriptions[[all_categories[1]]] <- c("Get the scientific name of a species from its common name",
      "following the NCBI database",
      "following EBI services",
      "following ITIS services",
      "following TROPICOS services",
      "following EOL services")
    all_descriptions[[all_categories[2]]] <- c("Scrape scientific names from a URL, text or any tipe of file",
      rep("using Global Names Recognition and Discovery (GNRD) services", 3),
      rep("using Taxon Finder",2))
    all_descriptions[[all_categories[3]]] <- c(
      "Match scientific names to authorative taxonomies and resolve mismatches",
      "using the Open Tree of Life taxonomy",
      "using the Global Names Resolver tool (several taxonomies)",
      "using iPlant collaborative services"
    )
    all_descriptions[[all_categories[4]]] <- c("Get all scientific names of species that:",
      "belong to a given higher taxon name",
      "and are found in a given country (using iNaturalist database)",
      "and have a genome sequence (deposited in NCBI)",
      "and match the most popular species within the taxon using OneZoom tool")
    all_descriptions[[all_categories[5]]] <- c("Get various information of a species such as",
      "image urls and corresponding license information using EOL",
      "information urls from EOL",
      "conservation status from ECO services")
    all_descriptions[[all_categories[6]]] <- c("Get phylogenetic trees from a list of taxa",
      "from Open Tree of Life synthetic tree", "and all supporting studies", "from Phylomatic", "from TreeBase", "using supersmart")
    all_descriptions[[all_categories[7]]] <- c("Scale branch lengths of a tree relative to time",
      "using the DateLife service",
      "using OToLs unoficial scaling service")
    all_descriptions[[all_categories[8]]] <- rep("Compare two phylogenetic trees symmetrically", 2)
    all_descriptions[[all_categories[9]]] <- rep("Save, publish, access, remove or update lists of names.", 5)
    return(all_descriptions)
}

make_table1 <- function(all_services, all_descriptions, image = TRUE){
    loadd(all_services)
    loadd(all_descriptions)
    table1 <- read.csv(file = "table1.csv", header = TRUE, skip = 0, stringsAsFactors = FALSE)
    rowsiesfoo <- function(x){ # function to get rows that will have an indent
      res <- length(x[[1]])
      for(i in 2:length(x)){
        res <- c(res, res[i-1] + length(x[[i]]))
      }
      res
    }
    # get rows that will have an indent:
    rowsies <- rowsiesfoo(all_services)
    rowsies2 <- seq(nrow(table1))
    remove <- rowsies-sapply(all_services, length)+1
    rowsies2 <- rowsies2[-remove]
    # line_sep <- rep("", nrow(table1))
    # line_sep[c(1, (remove[-1]-1))] <- "\\addlinespace" # found a better way to specify space between categories with group_rows
    table1 <- dplyr::mutate(table1, Web_Service = cell_spec(Web_Service, "latex", color =
            ifelse(seq(nrow(table1)) %in% remove, "red", "blue")),
            Description = cell_spec(Description, "latex", color =
                    ifelse(seq(nrow(table1)) %in% unique(c(remove, 18, 36:nrow(table1))), "black", "pink")))
    # escape set to FALSE when using cell_spec
    # linesep is used to override the default addition of a space every 5 lines
    t1 <- knitr::kable(table1, escape = FALSE, row.names = FALSE, format = "latex", booktabs = T, linesep = "")
        t1 <- gsub("\\\\textcolor\\{red\\}", "\\\\textbf", t1)
        t1 <- gsub("\\\\textcolor\\{blue\\}", "\\\\fontfamily{\\qcr\\}\\\\selectfont ", t1)
    # for(i in seq(length(remove))){
    #     pack_rows(t1, group_label = "", start_row = remove[i], end_row = rowsies[i], indent = FALSE)
    # } # cannot do pack_rows in a loop and it does not work outside the print either for some reason (may be related to the position of the pipe (has to be at the end of the line not beginning))
    EM <- "0.5em"
    kableExtra::kable_styling(t1, full_width = T, font_size = 7) %>% # latex_options = "scale_down",
        kableExtra::add_indent(rowsies2) %>%
        column_spec(1, width = "5cm") %>%
        column_spec(2, width = "8cm") %>%
        collapse_rows(columns = 2, latex_hline = "none", valign = "middle") %>%
        row_spec(0, bold = TRUE) -> table1_tex
    for (i in seq(length(remove))){
        table1_tex <- pack_rows(table1_tex, group_label = "", start_row = remove[i], end_row =
            rowsies[i], indent = FALSE, latex_gap_space = EM)
    }
    table1_tex <- gsub("Web\\_Service", "Web Service", table1_tex)
    table1_tex <- gsub("\\\\hspace\\{1em\\}", "\\\\hspace\\{1.5em\\}", table1_tex)
    table1_tex <- gsub("\\\\textcolor\\{pink\\}", "\\\\hspace\\{1em\\}", table1_tex)
    save_kable(table1_tex, file = "table1.png", keep_tex = TRUE)
    write(table1_tex, file = "table1.txt")
    return(table1_tex)
}
