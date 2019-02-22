# rphylotastic examples
# 1. extract names of species with certain characteristics from a pdf or a wikipedia page
# let's try carnivorous plants:
names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/Carnivorous_plant")
# 3. Resolve your scientific names
taxon_names <- taxa_resolve_names_with_otol(taxa = names_url[1:101])
write(paste(taxon_names, collapse = '", "'), file = "data-raw/test.txt")
taxon_names <- c("Drosera glanduligera", "Sarracenia", "Sarraceniaceae", "Triphyophyllum",
    "Nepenthes lowii", "Heliamphora chimantensis", "Darlingtonia", "Darlingtonia",
    "Darlingtonia", "Heliamphora")
# using all names gave an error, so trying with just 10 for now
# 4. Get the tree
phy1 <- taxa_get_otol_tree(taxa = taxon_names)
plot(phy1)
phy2 <- taxa_get_phylomatic_tree(taxa = taxon_names)
# Error: '{"error":"API rate limit exceeded","api-key":"160.36.155.220","count":"4","limit":"3"}
# ' does not exist in current working directory ('/Users/luna/Desktop/rphylotastic').
# this is an error from brranching::phylomatic_names
plot(phy2)
# 5. Now contextualize the plants of interest in the tree of all plants, just a sample of them:
# I already have a list of plants from different families to contextualize
install_github("LunaSare/phunding")
library(phunding)
data(plant_tree_orders)
str(plant_tree_orders)

taxa_convert_common_to_scientific(taxa = "flowering plants")
all_names <- c(plant_tree_orders$tip.label, taxon_names)
phy1 <- taxa_get_otol_tree(taxa = all_names)
