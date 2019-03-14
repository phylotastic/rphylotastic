# load_all("~/Desktop/datelife")
# rphylotastic examples
# 1. extract names of species with characteristics of interest from a pdf, a wikipedia page, etc.
# let's try it with blue flowers:
# names_url <- url_get_scientific_names(URL = "https://www.kapsenbergdesign.com/garden/byflowblue_bot.php")
# Error in open.connection(con, "rb") : HTTP error 500.
# names_url <- url_get_scientific_names(URL = "https://www.hunker.com/12000239/what-flowers-are-naturally-blue")
# not good enough data
# try with carnivorous plants
# names_url <- file_get_scientific_names(file_name = "~/Desktop/rphylotastic/data-raw/FNAfs_carnivory.pdf")
names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/List_of_carnivorous_plants")
names2 <- url_get_scientific_names("https://en.wikipedia.org/wiki/List_of_medicinal_plants_of_the_American_West")
# not necessary to do unique
# how many names did we extract from web page?
length(names_url) # > 1000
# 2. reduce your list to least inclusive clade above species:
names_url_reduced <- taxa_reduce_species(names_url)# there are 52 lineages
# 3. Resolve your scientific names
taxon_names <- taxa_resolve_names_with_otol(taxa = names_url_reduced)
length(taxon_names) # 42 are resolved with ott
# write(paste(taxon_names, collapse = '", "'), file = "data-raw/test.txt")
# 4. Get the tree of carnivorous lineages
phy1 <- taxa_get_otol_tree(taxa = taxon_names)
plot(phy1)
ape::Ntip(phy1)  # only 31 names are in otol tree
# we're not using phylomatic bc it's behaving weirdly; see tests
# 5. Now contextualize the plants of interest in the tree of angiosperms, just a sample of them:
data(terrestrial_plant_orders)
length(terrestrial_plant_orders)
all_names <- c(terrestrial_plant_orders, taxon_names)
# add list to portal's preloaded lists, for comparison:
# write(paste(all_names), file = "data-raw/all_names_list.txt")
# now get the tree from otol
phyall <- taxa_get_otol_tree(taxa = all_names)
plot(phyall, cex = 0.5)

tipcol <- rep("black", ape::Ntip(phyall))
mm <- match(gsub(" ", "_", taxon_names), phyall$tip.label)
sum(is.na(mm))
taxon_names[!is.na(mm)]
tipcol[mm[!is.na(mm)]] <- "blue"
plot(phyall, cex = 0.25, tip.color = tipcol)

bioblitz_species <- url_get_scientific_names(URL ="https://www.inaturalist.org/observations?project_id=24453&place_id=any&verifiable=any&captive=any&view=species")

yellowstone_birds <- url_get_scientific_names(URL="https://www.nps.gov/yell/learn/nature/upload/BirdChecklist2014.pdf")
us_birds <- taxon_get_species_from_country("Aves", country="US")
yellowstone_tree <- taxa_get_otol_tree(yellowstone_birds)
library(xml2)
library(rvest)
library(stringr)

birds="https://www.audubon.org/climate/national-parks/yellowstone-national-park"
doc <- read_html(birds)  %>% html_nodes("a")
common_names <- stringr::str_extract(as.character(doc), "/field-guide/bird/.*\"")
common_names <- common_names[!is.na(common_names)]
common_names <- gsub("/field-guide/bird/", "", common_names)
common_names <- gsub("\"", "", common_names)
common_names <- gsub("-", " ", common_names)
sci_names <- gsub(" ", "_", sapply(common_names[1:4], taxa_common_to_scientific))
tip.colors <- rep("black", length(yellowstone_tree$tip.label))
tip.colors[yellowstone_tree$tip.label %in% sci_names] <- "red"
ape::plot.phylo(yellowstone_tree, tip.color=tip.colors, cex=0.2)

birds_I_saw <- taxa_common_to_scientific(c("Osprey", "House sparrow", "Mallard duck", "American Robin"))
yellowstone_bird_tree <- taxa_get_otol_tree(url_get_scientific_names(URL="https://www.nps.gov/yell/learn/nature/upload/BirdChecklist2014.pdf"))
# ape::plot.phylo(yellowstone_bird_tree, tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"), cex=0.2)
ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree), tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"), cex=0.5, type = "fan")
# ape::is.binary(yellowstone_bird_tree)
# plotting a small tree:
yellowstone_bird_fams <- datelife::get_ott_clade(ott_ids = yellowstone_bird_tree$ott_ids, ott_rank = "family")
keep <- !duplicated(yellowstone_bird_fams$family) | yellowstone_bird_tree$tip.label%in%birds_I_saw
yellowstone_bird_fams$family[keep]
yellowstone_bird_fams$family[yellowstone_bird_tree$tip.label%in%birds_I_saw]
duplicated(yellowstone_bird_fams$family[keep])
yellowstone_bird_tree_small <- ape::drop.tip(yellowstone_bird_tree, yellowstone_bird_tree$tip.label[!keep])
pdf("data-raw/yellowstone_bird_tree_small_plot1.pdf", height = 5.5, width = 5.5)
par(xpd = TRUE)
par(mai = rep(1, 4))
ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree_small),
        tip.color=ifelse(yellowstone_bird_tree_small$tip.label%in%birds_I_saw, "red", "black"),
        cex=0.5, type = "fan", label.offset = 0.02)
dev.off()
# plotting clade labels:
families <- unique(names(yellowstone_bird_fams$family))
tipsies <- sapply(families, function(x) yellowstone_bird_tree$tip.label[names(yellowstone_bird_fams$family)%in%x])
nodes <- sapply(families, function(x)
                          phytools::findMRCA(tree = yellowstone_bird_tree,
                                             tips = tipsies[[i]],
                                             type= "node"))
# orient <- c("curved", rep("horizontal",3))

source("data-raw/arclabels.R")
CEX <- 0.28
EW <- 0.45
LO <- 0.02
pdf("data-raw/yellowstone_bird_tree_plot1.pdf", height = 5.5, width = 5.5)
par(xpd = TRUE)
par(mai = rep(1, 4))
ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree),
        tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"),
        cex=CEX, type = "fan", edge.width = EW, label.offset = LO)
dev.off()

pdf("data-raw/yellowstone_bird_tree_plot2.pdf", height = 6.5, width = 6.5)
par(xpd = TRUE)
par(mai = rep(1, 4))
yellowstone_bird_tree_fams <- yellowstone_bird_tree
yellowstone_bird_tree_fams$tip.label <- names(yellowstone_bird_fams$family)
ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree_fams),
        tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"),
        cex=0.3, type = "fan", edge.width = EW, label.offset = LO)
dev.off()

for(i in seq(length(tipsies))){
    pdf(paste0("data-raw/yellowstone_bird_tree_plot_test_", i, ".pdf"), height = 5.5, width = 5.5)
    par(xpd = TRUE)
    par(mai = rep(1, 4))
    ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree_fams),
            tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"),
            cex=0.3, type = "fan", edge.width = EW, label.offset = LO)
    if(length(tipsies[[i]]) > 1){
        arclabels(tree = yellowstone_bird_tree, text = families[i], tips = tipsies[[i]], orientation = "curved",
                                lab.offset = 1.3,ln.offset=1.2, cex = 0.5)
    }
    dev.off()
}


arc_label_offset <- rep(1.6, length(tipsies))
arc_label_offset[c(1,4)] <- 1.4
arc_label_offset[19] <- 1.35
arc_label_offset[22] <- 1.33
arc_colors <- sample(rainbow(n = length(tipsies)), length(tipsies))
pdf(paste0("data-raw/yellowstone_bird_tree_plot_test_all.pdf"), height = 5.5, width = 5.5)
par(xpd = TRUE)
par(mai = rep(1, 4))
ape::plot.phylo(ape::compute.brlen(yellowstone_bird_tree),
        tip.color=ifelse(yellowstone_bird_tree$tip.label%in%birds_I_saw, "red", "black"),
        cex=CEX, type = "fan", edge.width = EW, label.offset = LO)
    for(i in seq(length(tipsies))){
        if(length(tipsies[[i]]) > 1){
            cat(i, families[i], "\n")
            arclabels(tree = yellowstone_bird_tree, text = NULL, tips = tipsies[[i]], orientation = "curved",
                      col = arc_colors[i], lwd = 4, lab.offset = arc_label_offset[i],ln.offset=arc_label_offset[i]-0.05, cex = 0.5)
        }
    }
    # mapply(arclabels, tree = yellowstone_bird_tree, text = families[i], tips = tipsies[[i]], orientation = "curved",
    #                         lab.offset = arc_label_offset[i],ln.offset=arc_label_offset[i]-0.05, cex = 0.5))
dev.off()


# other examples:
plants_I_own <- taxa_common_to_scientific(c("venus flytrap", "pitcher plant", "california pitcherplant", "sundew"))
taxa_common_to_scientific("monocotyledon")
taxa_common_to_scientific("monocots")
