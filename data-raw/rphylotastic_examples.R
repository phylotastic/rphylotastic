# install("~/Desktop/datelife")
# install_github("phylotastic/datelife")
# install_github("phylotastic/rphylotastic")
# library(ape)
# library(drake)
# source("R/arclabels.R")
# source("data-raw/rphylotastic_examples_functions.R")
# source("data-raw/rphylotastic_examples.R")
# carnivplants_plan <- drake_plan(
#     # 1. extract names of species with characteristics of interest from a pdf, a wikipedia page, etc.
#     # let's try it with blue flowers:
#     # names_url <- url_get_scientific_names(URL = "https://www.kapsenbergdesign.com/garden/byflowblue_bot.php")
#     # Error in open.connection(con, "rb") : HTTP error 500.
#     # names_url <- url_get_scientific_names(URL = "https://www.hunker.com/12000239/what-flowers-are-naturally-blue")
#     # not good enough data
#     # try with carnivorous plants
#     # names_url <- file_get_scientific_names(file_name = "~/Desktop/rphylotastic/data-raw/FNAfs_carnivory.pdf")
#     names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/List_of_carnivorous_plants")
#     names2 <- url_get_scientific_names("https://en.wikipedia.org/wiki/List_of_medicinal_plants_of_the_American_West")
#     # not necessary to do unique
#     # how many names did we extract from web page?
#     length(names_url) # > 1000
#     # 2. reduce your list to least inclusive clade above species:
#     names_url_reduced <- taxa_reduce_species(names_url)# there are 52 lineages
#     # 3. Resolve your scientific names
#     taxon_names <- taxa_resolve_names_with_otol(taxa = names_url_reduced)
#     length(taxon_names) # 42 are resolved with ott
#     # write(paste(taxon_names, collapse = '", "'), file = "data-raw/test.txt")
#     # 4. Get the tree of carnivorous lineages
#     phy1 <- taxa_get_otol_tree(taxa = taxon_names)
#     plot(phy1)
#     ape::Ntip(phy1)  # only 31 names are in otol tree
#     # we're not using phylomatic bc it's behaving weirdly; see tests
#     # 5. Now contextualize the plants of interest in the tree of angiosperms, just a sample of them:
#     data(terrestrial_plant_orders)
#     length(terrestrial_plant_orders)
#     all_names <- c(terrestrial_plant_orders, taxon_names)
#     # add list to portal's preloaded lists, for comparison:
#     # write(paste(all_names), file = "data-raw/all_names_list.txt")
#     # now get the tree from otol
#     phyall <- taxa_get_otol_tree(taxa = all_names)
#     plot(phyall, cex = 0.5)
#
#     tipcol <- rep("black", ape::Ntip(phyall))
#     mm <- match(gsub(" ", "_", taxon_names), phyall$tip.label)
#     sum(is.na(mm))
#     taxon_names[!is.na(mm)]
#     tipcol[mm[!is.na(mm)]] <- "blue"
#     plot(phyall, cex = 0.25, tip.color = tipcol)
# )
# bird_plan_try1 <- drake_plan(
#     bioblitz_species <- url_get_scientific_names(URL ="https://www.inaturalist.org/observations?project_id=24453&place_id=any&verifiable=any&captive=any&view=species")
#
#     yellowstone_birds <- url_get_scientific_names(URL="https://www.nps.gov/yell/learn/nature/upload/BirdChecklist2014.pdf")
#     us_birds <- taxon_get_species_from_country("Aves", country="US")
#     yellowstone_tree <- taxa_get_otol_tree(yellowstone_birds)
#     library(xml2)
#     library(rvest)
#     library(stringr)
#
#     birds="https://www.audubon.org/climate/national-parks/yellowstone-national-park"
#     doc <- read_html(birds)  %>% html_nodes("a")
#     common_names <- stringr::str_extract(as.character(doc), "/field-guide/bird/.*\"")
#     common_names <- common_names[!is.na(common_names)]
#     common_names <- gsub("/field-guide/bird/", "", common_names)
#     common_names <- gsub("\"", "", common_names)
#     common_names <- gsub("-", " ", common_names)
#     sci_names <- gsub(" ", "_", sapply(common_names[1:4], taxa_common_to_scientific))
#     tip.colors <- rep("black", length(yellowstone_tree$tip.label))
#     tip.colors[yellowstone_tree$tip.label %in% sci_names] <- "red"
#     ape::plot.phylo(yellowstone_tree, tip.color=tip.colors, cex=0.2)
# )

bird_plan_final <- drake_plan(
    strings_in_dots = "literals",
    birds_I_saw = taxa_common_to_scientific(c("Osprey",
        "House sparrow", "Mallard duck", "American Robin",
        "song sparrow", "mourning dove", "house wren")),
    # alternative function, works slightly worse:
    # taxize::comm2sci(c("wren", "Osprey", "House sparrow", "Mallard duck", "American Robin"))
    yellowstone_birds = url_get_scientific_names(URL =
      "https://www.nps.gov/yell/learn/nature/upload/BirdChecklist2014.pdf"),
    yellowstone_bird_tree1 = taxa_get_otol_tree(yellowstone_birds),
    yellowstone_bird_tree = datelife::datelife_search(yellowstone_bird_tree1,
         summary_format = "phylo_median"),
    # usethis::edit_r_environ() # to set ENTREZ_KEY in file and avoid rate limits
    # yellowstone_bird_tree_common_names = sapply(gsub("_", " ",
    #     yellowstone_bird_tree$tip.label),
    #     function(x) try(taxize::sci2comm(x))),
    tree_graph0 = write_plot(yellowstone_bird_tree, "0", width = 2.5, cex = 0.18),
    tree_graph1 = write_plot(yellowstone_bird_tree, "1", mai = rep(0.4,4),
        cex= 0.3, type = "fan", edge.width = 0.45, label.offset = 1.5),
    yellowstone_bird_fams = datelife::get_ott_clade(ott_ids = yellowstone_bird_tree$ott_ids,
        ott_rank = "family"),
    yellowstone_bird_tree_fams = get_fams_on_tips(yellowstone_bird_tree, yellowstone_bird_fams),
    tree_graph1fams = write_plot(yellowstone_bird_tree_fams, "1fams", mai = rep(0.4,4),
        cex= 0.3, type = "fan", edge.width = 0.45, label.offset = 1.5, tip.color =
        ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black")),
    # plot small tree:
    keep = !duplicated(yellowstone_bird_fams$family) | yellowstone_bird_tree$tip.label%in%birds_I_saw,
    # yellowstone_bird_fams$family[keep]
    # yellowstone_bird_fams$family[yellowstone_bird_tree$tip.label%in%birds_I_saw]
    # duplicated(yellowstone_bird_fams$family[keep])
    yellowstone_bird_tree_small = ape::drop.tip(yellowstone_bird_tree,
        yellowstone_bird_tree$tip.label[!keep]),
    tree_graph1small = write_plot(yellowstone_bird_tree_small, "1small", mai = rep(1,4),
        cex= 0.5, type = "fan", edge.width = 0.9, label.offset = 2),
    # plotting clade labels:
    families = unique(names(yellowstone_bird_fams$family)),
    tipsies = sapply(families, function(x) yellowstone_bird_tree$tip.label[names(yellowstone_bird_fams$family)%in%x]),
    # nodes = sapply(families, function(x) phytools::findMRCA(tree = yellowstone_bird_tree,
    #     tips = tipsies[[x]], type= "node"))
    tree_graph2_tests = graph2_tests(yellowstone_bird_tree, birds_I_saw, families, tipsies),
    arc_line_offset = rep(1.63, length(tipsies)),
    arc_label_offset = albo(arc_line_offset),
    seede = set.seed(100),
    arc_rainbow = sample(rainbow(n = length(tipsies)), length(tipsies)),
    tree_graph2rainbow = write_plot(yellowstone_bird_tree, "2_rainbow", mai = rep(1,4),
        cex= 0.3, type = "fan", edge.width = 0.45, label.offset = 1.5,
        arclabelspars = list(tips = tipsies, text = families, arc.line.offset = arc_line_offset,
        arc.label.offset = arc_label_offset, arc.cols = arc_rainbow)),
    arc_grays = sample(gray.colors(n = length(tipsies)), length(tipsies)),
    tree_graph2grayA = write_plot(yellowstone_bird_tree, "2_grayscale-A", mai = rep(1,4),
        cex= 0.3, type = "fan", edge.width = 0.45, label.offset = 1.5,
        arclabelspars = list(tips = tipsies, text = families, arc.line.offset = arc_line_offset,
        arc.label.offset = arc_label_offset, arc.cols = arc_grays)),
    arc_label_offset2 = albo2(arc_line_offset),
    tree_graph2grayB = write_plot(yellowstone_bird_tree, "2_grayscale-B", mai = rep(1,4),
        cex= 0.3, type = "fan", edge.width = 0.45, label.offset = 1.5,
        arclabelspars = list(tips = tipsies, text = families, arc.line.offset = arc_label_offset2 - 0.05,
        arc.label.offset = arc_label_offset2, arc.cols = arc_grays))
)



make(bird_plan_final)

# other examples:
# plants_I_own <- taxa_common_to_scientific(c("venus flytrap", "pitcher plant", "california pitcherplant", "sundew"))
# taxa_common_to_scientific("monocotyledon")
# taxa_common_to_scientific("monocots")
#
# #joddie rphylotastic examples
# my_taxa <- c("Chrotaphytus collaris", "Anolis carolinensis", "Agama Agama", "Leiocephalus personatus")
#
# iguania_tree <- taxa_get_otol_tree(url_get_scientific_names(URL="https://en.wikipedia.org/wiki/Iguanomorpha"))
#
# ape::plot.phylo(iguania_tree, tip.color=ifelse(iguania_tree$tip.label%in%birds_I_saw, "orange", "black"), cex=0.2)
