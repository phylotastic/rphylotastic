load_all("~/Desktop/datelife")
# rphylotastic examples
# 1. extract names of species with certain characteristics from a pdf or a wikipedia page
# let's try it with blue flowers:
names_url <- url_get_scientific_names(URL = "https://www.kapsenbergdesign.com/garden/byflowblue_bot.php")
names_url <- unique(names_url)
length(names_url) # 80
# 3. Resolve your scientific names
taxon_names <- taxa_resolve_names_with_otol(taxa = names_url)
# taxon_names <- taxa_resolve_names_with_gnr(taxa = names_url)
length(taxon_names) # 73
# write(paste(taxon_names, collapse = '", "'), file = "data-raw/test.txt")
# using all names gave an error, so trying with just 10 for now
# 4. Get the tree
phy1 <- taxa_get_otol_tree(taxa = taxon_names)
mm1 <- match(gsub(" ", "_", taxon_names), phy1$tip.label)
taxon_names[is.na(mm1)]  #25 spp are not in otol tree
ape::Ntip(phy1)  #59
plot(phy1)
# we're not using phylomatic bc it's behaving weirdly; see tests
# 5. Now contextualize the plants of interest in the tree of angiosperms, just a sample of them:
data(flower_plant_fams)
all_names <- c(flower_plant_fams, taxon_names)
phyall <- taxa_get_otol_tree(taxa = all_names)
if(inherits(phyall, "phylo")){
    phyall <- ape::reorder.phylo(ape::collapse.singles(phyall))
    phyall$tip.label <- gsub("_ott.*", "", phyall$tip.label)
    phyall$tip.label <- gsub("\\(.*", "", phyall$tip.label)
}
mrca_index <- grep("mrcaott", phyall$tip.label)
# find the actual scientific name of mrca ott names:
mrca_ottids <- gsub("mrcaott.*ott", "", phyall$tip.label[mrca_index])
mrca_lin <- datelife::get_ott_lineage(ott_ids = as.numeric(mrca_ottids))
i=1
mrca_famnames <- sapply(seq_along(mrca_lin), function(i) rownames(mrca_lin[[i]])[mrca_lin[[i]][ ,"ott_ranks"] == "family"])
phyall$tip.label[mrca_index] <- mrca_famnames
tipcol <- rep("black", ape::Ntip(phyall))
mm <- match(gsub(" ", "_", taxon_names), phyall$tip.label)
taxon_names[is.na(mm)]
tipcol[mm[!is.na(mm)]] <- "red"
plot(phyall, cex = 0.5, tip.color = tipcol)
plot(plant_tree_orders, cex = 0.5)
rotl::taxonomy_taxon_info(454749)
lin <- datelife::get_ott_lineage(ott_ids = 454749)
lin <- datelife::get_ott_lineage(ott_ids = 508293)
