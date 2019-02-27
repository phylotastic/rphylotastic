test_that("Loading a tree from OToL", {
  taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae", "Formica polyctena", "Tetramorium caespitum", "Pseudomyrmex", "Carebara diversa", "Formicinae")
  tree <- taxa_get_otol_tree(taxa)
  expect_equal(class(tree), "phylo")
  expect_gte(ape::Ntip(tree), 5)
})

test_that("Loading a tree from Phylomatic", {
  taxa <- c("Panthera leo","Panthera onca","Panthera tigris","Panthera uncia")
  tree <- taxa_get_phylomatic_tree(taxa)
  expect_equal("phylo", class(tree))
  tree2 <- taxa_get_phylomatic_tree(taxa = c("elephas maximus", "felis silvestris", "homo sapiens", "delphinus delphus"))
  expect_true(any(grepl("Homo", tree2$tip.label)))
  tree3 <- taxa_get_phylomatic_tree(taxa = c("elephas", "felis", "homo", "delphinus"))
  # for some reason when using genus names and not species names, tip.label names are empty and the following is failing
  skip_on_cran()
  skip_on_travis()
  skip(message = "still need to fix dropped names for higher taxa in taxa_get_phylomatic_tree")
  expect_true(any(grepl("Homo", tree3$tip.label)))
})


# test_that("Loading a tree from NCBI taxonomy", {
#   taxa <- c("Setophaga striata","Setophaga magnolia","Setophaga angelae","Setophaga plumbea","Setophaga virens")
#   tree <- taxa_get_taxonomic_tree(taxa)
#   expect_equal("phylo", class(tree))
# })

test_that("get_phylomatic tree works"){
    # let's try the wikipedia list of carnivorous plants:
    # names_url <- url_get_scientific_names(URL = "https://en.wikipedia.org/wiki/Carnivorous_plant")
    # names_url <- unique(names_url)
    # write(paste(names_url, collapse = '", "'), file = "data-raw/test.txt")
    taxon_names <- c("Nepenthes lowii", "Heliamphora chimantensis", "Triphyophyllum", "Drosera glanduligera", "Sarraceniaceae", "Darlingtonia", "Heliamphora", "Sarracenia", "Nepenthaceae", "Cephalotaceae", "Cephalotus", "Eriocaulaceae", "Paepalanthus", "Bromeliaceae", "Brocchinia", "Catopsis", "Ericales", "Sarracenia purpurea subsp. purpurea", "Darlingtonia californica", "Sarracenia flava", "Sarracenia psittacina", "Sarracenia minor", "Brocchinia reducta", "Nepenthes rajah", "Nepenthes bicalcarata", "Cephalotus follicularis", "Pinguicula conzattii", "Drosera capensis", "Pinguicula", "Drosera", "D. burmanii", "D. pygmaea", "D. peltata", "Drosophyllum", "Byblis", "Triphyophyllum peltatum", "Dioncophyllaceae", "Droseraceae", "Ancistrocladaceae", "Plumbaginaceae", "Dionaea muscipula", "Venus flytrap", "Aldrovanda", "Dionaea", "Utricularia vulgaris", "Utricularia", "Daphnia", "U. sandersonii", "U. macrorhiza", "Genlisea violacea", "Genlisea", "Nepenthes aristolochioides", "Protocarnivorous", "Roridula gorgonias", "Roridula", "Pameridea", "Martyniaceae", "Pedaliaceae", "Ibicella lutea", "Paepalanthus bromelioides [27] bracts", "Passiflora foetida", "Stylidium", "Catopsis berteroniana", "Caryophyllales", "Polygonaceae", "Limonium", "Ceratostigma", "Drosophyllum lusitanicum", "Sphagnum", "Pinguicula valisneriifolia", "Pinguicula vulgaris", "Sarracenia spp", "Nepenthes mirabilis", "Utricularia macrorhiza", "Venus flytraps", "Microhyla nepenthicola", "Misumenops nepenthicola", "Wyeomyia smithii", "Pameridea roridulae", "Nepenthes infauna", "Camponotus schmitzi", "Toxorhynchites", "Geosesarma malayanum", "Tupaia montana", "Rattus baluensis", "Kerivoula hardwickii", "Nepenthes hemsleyana", "Nepenthales", "Sarraceniales", "Byblidaceae", "Roridulaceae", "Saxifragales", "Lentibulariaceae", "Scrophulariales", "Stylidium turbinatum", "Aldrovanda vesiculosa", "Byblis liniflora", "Asterales", "Stylidiaceae", "Drosophyllaceae", "Droseridites", "Anurosperma", "Lamiales", "Utricularia bladderworts", "Polypompholyx", "Biovularia", "Ibicella", "Oxalidales", "Poales", "Paepalanthus bromelioides", "Botrytis cinerea", "Drosera binata", "Pinguicula grandiflora", "Pinguicula moranensis", "Sudarshana", "Stylidium spp", "Sarraceliaceae", "Ancistrocladus", "Oecologia", "Drosera intermedia", "Sarracenia purpurea", "Blumea", "Taina", "Nepenthes khasiana", "Palaeoaldrovanda", "Philcoxia", "Aracamunia", "Capsella", "Colura", "Dipsacus", "Drymocallis", "Lathraea", "Passiflora", "Proboscidea", "Latina", "Bahasa", "Carnivorous", "Monocots")
    taxa.string <- brranching::phylomatic_names(taxon_names, format = "isubmit")

    phy <- taxa_get_phylomatic_tree(taxa = taxon_names[1:10]) # result is a tree with 7 tips
    expect_true(ape::Ntip(phy) == 7)
    phy2 <- taxa_get_phylomatic_tree(taxa = taxon_names) # results in one name: Tupaia montana
    expect_true(ape::Ntip(phy2) > 7)
}
