# rphylotastic examples
# 1. extract names of species with certain characteristics from a pdf or a wikipedia page
# let's try carnivorous plants:
names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/Carnivorous_plant")
# 3. Resolve your scientific names
taxon_names <- taxa_resolve_names_with_otol(taxa = names_url[1:10]) 
# using all names gave an error, so trying with just 10 for now
# 4. Get the tree
phy1 <- taxa_get_otol_tree(taxa = taxon_names)
plot(phy1)
phy2 <- taxa_get_phylomatic_tree(taxa = taxon_names)
# Error: '{"error":"API rate limit exceeded","api-key":"160.36.155.220","count":"4","limit":"3"}
# ' does not exist in current working directory ('/Users/luna/Desktop/rphylotastic').
# this is an error from brranching::phylomatic_names
plot(phy2)
# 5. Now contextualize the plants of interest in the tree of all plants, just a sample of them
