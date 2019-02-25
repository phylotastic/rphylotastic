load_all("~/Desktop/datelife")
# rphylotastic examples
# 1. extract names of species with certain characteristics from a pdf or a wikipedia page
# let's try it with blue flowers:
# names_url <- url_get_scientific_names(URL = "https://www.kapsenbergdesign.com/garden/byflowblue_bot.php")
# Error in open.connection(con, "rb") : HTTP error 500.
# names_url <- url_get_scientific_names(URL = "https://www.hunker.com/12000239/what-flowers-are-naturally-blue")
# not good enough data
# names_url <- file_get_scientific_names(file_name = "~/Desktop/rphylotastic/data-raw/FNAfs_carnivory.pdf")
names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/List_of_carnivorous_plants")
names_url <- unique(names_url)
length(names_url) # > 1000
genera <- unique(sapply(strsplit(names_url, " "), "[", 1)) # there are 38 genera
# 3. Resolve your scientific names
taxon_names <- taxa_resolve_names_with_otol(taxa = genera)
# taxon_names <- taxa_resolve_names_with_gnr(taxa = names_url)
length(taxon_names) # 36 are found in ott
# write(paste(taxon_names, collapse = '", "'), file = "data-raw/test.txt")
# using all names gave an error, so trying with just 10 for now
# 4. Get the tree of the carnivorous genera
phy1 <- taxa_get_otol_tree(taxa = taxon_names)
plot(phy1)
ape::Ntip(phy1)  # only 31 spp are in otol tree
phy1$tip.label
# we're not using phylomatic bc it's behaving weirdly; see tests
# 5. Now contextualize the plants of interest in the tree of angiosperms, just a sample of them:
data(flower_plant_fams)
all_names <- c(flower_plant_fams, taxon_names)
phyall <- taxa_get_otol_tree(taxa = all_names)
plot(phyall, cex = 0.5)
phyall <- ape::reorder.phylo(ape::collapse.singles(phyall))
phyall$tip.label <- gsub("_ott.*", "", phyall$tip.label)
phyall$tip.label <- gsub("\\(.*", "", phyall$tip.label)

mrca_index <- grep("mrcaott", phyall$tip.label)
# find the actual scientific name of mrca ott names:
mrca_ottids <- gsub("mrcaott.*ott", "", phyall$tip.label[mrca_index])
mrca_lin <- datelife::get_ott_lineage(ott_ids = as.numeric(mrca_ottids))
mrca_famnames <- sapply(seq_along(mrca_lin), function(i) rownames(mrca_lin[[i]])[mrca_lin[[i]][ ,"ott_ranks"] == "family"])
phyall$tip.label[mrca_index] <- mrca_famnames
tipcol <- rep("black", ape::Ntip(phyall))
mm <- match(gsub(" ", "_", taxon_names), phyall$tip.label)
sum(is.na(mm))
taxon_names[!is.na(mm)]
tipcol[mm[!is.na(mm)]] <- "blue"
plot(phyall, cex = 0.25, tip.color = tipcol)
