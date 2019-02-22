#' Get OToL induced subtree
#'
#' @param taxa The vector of names, already resolved to match OToL taxa
#' @return A phylo object
#' @examples
#' taxa <- c("Crabronidae", "Ophiocordyceps", "Megalyridae",
#'           "Formica polyctena", "Tetramorium caespitum",
#'           "Pseudomyrmex", "Carebara diversa", "Formicinae")
#' phy <- taxa_get_otol_tree(taxa)
#' plot(phy)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the rotl package, another interface to Open Tree of Life
#' @export
taxa_get_otol_tree <- function(taxa) {
    # GET method; chokes with mor ethan 105 taxa:
  # taxa.string <- utils::URLencode(paste(taxa, collapse='"|"'))
  # results <- jsonlite::fromJSON(paste0(get_base_url(), 'gt/ot/get_tree?taxa=', taxa.string))$newick
  # tmp.file <- paste0(tempdir(), "/tmp.tre")
  # cat(results, file=tmp.file)
  # tree <- ape::reorder.phylo(ape::collapse.singles(methods::as(phylobase::readNewick(tmp.file), "phylo")))
  # POST method; works well so far:
  taxa.string <- paste(taxa, collapse='", "')
  postcall <- paste0('{"taxa": ["', taxa.string, '"]}')
   postcall <- paste0("curl -X POST '", get_base_url(), "gt/ot/tree' -H 'content-type:application/json' -d '", postcall, "'")
  results <- jsonlite::fromJSON(system(postcall, intern=TRUE))
  tree <- tryCatch(ape::read.tree(text=results$newick), error = function(e){
    # we could use phytools::read.newick instead, it is better also for handling singleton nodes
    # but if text has one tip only, it just stays running forever, so we will stay with ape::read.tree for now.
    message("\n Provided taxa are not in otol tree.\n")
    results$newick
  })
  if(inherits(tree, "phylo")){
      tree <- ape::reorder.phylo(ape::collapse.singles(tree))
  }
  return(tree)
}


#' Get phylomatic subtree
#'
#' @param taxa The vector of names, already resolved
#' @return A phylo object
#' @examples
#' phy <- taxa_get_phylomatic_tree(c("Panthera leo", "Panthera onca",
#'          "Panthera tigris", "Panthera uncia"))
#' plot(phy)
#' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the interface of phylomatic \url{http://phylodiversity.net/phylomatic/}
#' @export
taxa_get_phylomatic_tree <- function(taxa) {
    # taxa.string <- vector(mode = "character", length = length(taxa))
    # progression <- utils::txtProgressBar(min = 0, max = length(taxa), style = 3)
    # for(i in seq_along(taxa)){
        taxa.string <- brranching::phylomatic_names(taxa, format = "isubmit")
        # taxa.string[i] <- suppressMessages(brranching::phylomatic_names(taxa[i], format = "isubmit")) # this step is necessary to clean names for phylomatic
        # it's giving trouble at taxize::tax_name:
        # Error: '{"error":"API rate limit exceeded","api-key":"160.36.155.220","count":"4","limit":"3"}
        # ' does not exist in current working directory ('/Users/luna/Desktop/rphylotastic').
        # so adding sys.sleep to limit calls to ncbi api
    #     Sys.sleep(0.33)
    #     utils::setTxtProgressBar(progression, i)
    # }
    # cat("\n")
    # it was giving trouble because I used suppressMessages and taxon matching decisions would not be shown, exceeding wait time for the function...
    taxa.string2 <- taxa.string
    taxa.string <- taxa.string2[1:100]
  # get method; chokes with more than 105 names too:
  # taxa.string <- utils::URLencode(paste(taxa.string, collapse="|"))  # this is crucial to process query
  # results <- jsonlite::fromJSON(paste0(get_base_url(), 'gt/pm/get_tree?taxa=', taxa.string))
  # tree <- tryCatch(ape::read.tree(text=results$newick), error = function(e){
  #   # we could use phytools::read.newick instead, it is better also for handling singleton nodes
  #   # but if text has one tip only, it just stays running forever, so we will stay with ape::read.tree for now.
  #   message("\n Phylomatic returned a tree with one tip only.\n taxa_get_phylomatic_tree output is not a phylo object.")
  #   results$newick
  # })
  # Post method: it works, but it is behaving weirdly:
  # try code in rphylotastic_examples to see how
  taxa.string <- paste(taxa.string, collapse='", "')
  postcall <- paste0('{"taxa": ["', taxa.string, '"]}')
  postcall <- paste0("curl -X POST '", get_base_url(), "gt/pm/tree' -H 'content-type:application/json' -d '", postcall, "'")
  results <- jsonlite::fromJSON(system(postcall, intern=TRUE))
  tree <- tryCatch(ape::read.tree(text=results$newick), error = function(e){
    # we could use phytools::read.newick instead, it is better also for handling singleton nodes
    # but if text has one tip only, it just stays running forever, so we will stay with ape::read.tree for now.
    message("\n Provided taxa are not enough to get a phylomatic tree.\n")
    results$newick
  })
  if(inherits(tree, "phylo")){
      tree <- ape::reorder.phylo(ape::collapse.singles(tree))
  }
  return(tree)
}


# #' Get taxonomic tree from the NCBI
# #'
# #' @param taxa The vector of names, already resolved
# #' @return A phylo object
# #' @examples
# #' taxa <- c("Setophaga striata", "Setophaga magnolia",
# #'      "Setophaga angelae", "Setophaga plumbea",
# #'      "Setophaga virens")
# #' phy <- taxa_get_taxonomic_tree(taxa)
# #' plot(phy)
# #' @seealso \url{https://github.com/phylotastic/phylo_services_docs/tree/master/ServiceDescription} or the interface of phyloT \url{http://phylot.biobyte.de/}
# #' @export
# taxa_get_taxonomic_tree <- function(taxa) {
#   taxa.string <- utils::URLencode(paste(taxa, collapse="|"))
#   results <- jsonlite::fromJSON(paste0(get_base_url(), 'gt/pt/get_tree?taxa=', taxa.string))
#
#   tree <- ape::read.tree(text=results$newick)
#
#   return(tree)
# }
#
# Code that's giving:
# Error: '{"error":"API rate limit exceeded","api-key":"160.36.155.220","count":"4","limit":"3"}
# ' does not exist in current working directory ('/Users/luna/Desktop/rphylotastic').
# phylomatic_names <- function(taxa = taxon_names, format='isubmit', db="ncbi") {
# format <- match.arg(format, c('isubmit', 'rsubmit'))
# db <- match.arg(db, c('ncbi', 'itis', 'apg'))
# nnn <- taxon_names[1]
# traits_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
# cap <- function(s) {
#   paste(toupper(substring(s, 1, 1)), {
#     s <- substring(s,2)
#     if (strict) tolower(s) else s
#   }, sep = "", collapse = " ")
# }
# if (!onlyfirst) {
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# } else {
#   sapply(s, function(x) {
#     paste(toupper(substring(x, 1, 1)),
#           tolower(substring(x, 2)),
#           sep = "", collapse = " ")
#   }, USE.NAMES = FALSE)
# }
# }
# foo <- function(nnn) {
#   # split up strings if a species name
#   nnn <- iconv(nnn, from = "ISO_8859-2", to = "UTF-8")
#   taxa2 <- strsplit(gsub("_"," ",nnn), "\\s")[[1]]
#   taxa_genus <- traits_capwords(taxa2[[1]], onlyfirst = TRUE)
#
#   if (db %in% c("ncbi", "itis")) {
#     family <- taxize::tax_name(query = taxa_genus, get = "family", db = db)$family
#   } else {
#     tplfamily <- tpl[ match(taxa_genus, tpl$genus), "family" ]
#     dd <- taxize::apg_families[ match(tplfamily, taxize::apg_families$this), ]
#     if (nchar(as.character(dd$that), keepNA = FALSE) == 0) {
#       family <- dd$this
#     } else {
#       family <- dd$that
#     }
#   }
#   stringg <- c(family, strsplit(nnn, " ")[[1]])
#   stringg <- tolower(as.character(stringg))
#   if (format == 'isubmit') {
#     paste(stringg[[1]], "/", stringg[2], "/", tolower(sub(" ", "_", nnn)), sep = '')
#   } else
#     if (format == 'rsubmit') {
#       paste(stringg[[1]], "%2F", stringg[2], "%2F", tolower(sub(" ", "_", nnn)), sep = '')
#     }
# }
# sapply(taxon_names, foo, USE.NAMES = FALSE)
# }
# for(i in taxon_names){
#     family <- taxize::tax_name(query = i, get = "family", db = db)$family
#     Sys.sleep(0.33)
# }
# repeat_until_it_works <- function(catch, path, query, max_tries = 10,
#   wait_time = 100, messages = TRUE, ...) {
#
#   error_handler <- function(e) {
#     if (e$message %in% catch) {
#       if (messages) warning(paste("Caught error:", e$message))
#       return(NA)
#     } else {
#       stop(e$message)
#     }
#   }
#   for (count in 1:max_tries) {
#     cli <- crul::HttpClient$new(url = ncbi_base(), opts = list(...))
#     res <- cli$get(sprintf("entrez/eutils/%s.fcgi", path),
#       query = tc(query))
#     output <- tryCatch(res$parse("UTF-8"), error = error_handler)
#     if (!is.na(output)) return(output)
#     Sys.sleep(wait_time * count)
#   }
#   return(output)
# }
#
# get_uid <- function(sciname, ask = TRUE, messages = TRUE, rows = NA,
#                     modifier = NULL, rank_query = NULL,
#                     division_filter = NULL, rank_filter = NULL,
#                     key = NULL, ...) {
#
#   assert(ask, "logical")
#   assert(messages, "logical")
#   assert(modifier, "character")
#   assert(rank_query, "character")
#   assert(division_filter, "character")
#   assert(rank_filter, "character")
#   if (!is.na(rows)) {
#     assert(rows, c("numeric", "integer"))
#     stopifnot(rows > 0)
#   }
#   key <- getkey(key, service = "entrez")
#
#   fun <- function(sciname, ask, messages, rows, ...) {
#     direct <- FALSE
#     mssg(messages, "\nRetrieving data for taxon '", sciname, "'\n")
#     sciname <- gsub(" ", "+", sciname)
#     if (!is.null(modifier))
#       sciname <- paste0(sciname, sprintf("[%s]", modifier))
#     term <- sciname
#     if (!is.null(rank_query))
#       term <- paste0(term, sprintf(" AND %s[Rank]", rank_query))
#     try_again_errors <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
#     query_args <- tc(list(db = "taxonomy", term = term, api_key = key))
#     raw_xml_result <- repeat_until_it_works(try_again_errors,
#                                             "esearch",
#                                             query = query_args,
#                                             ...)
#     xml_result <- xml2::read_xml(raw_xml_result)
#
#     # NCBI limits requests to three per second when no key
#     if (is.null(key)) Sys.sleep(0.33)
#     uid <- xml2::xml_text(xml2::xml_find_all(xml_result, "//IdList/Id"))
#     mm <- length(uid) > 1
#
#     if (length(uid) == 0) { # if taxon name is not found
#       uid <- NA_character_
#     } else {
#       att <- 'found'
#     }
#
#     # not found on ncbi
#     if (length(uid) == 0 || all(is.na(uid))) {
#       mssg(messages, m_not_found_sp_altclass)
#       uid <- NA_character_
#       att <- 'NA due to not found'
#     }
#     # more than one found on ncbi -> user input
#     if (length(uid) > 1) {
#       ID <- paste(uid, collapse = ",")
#       try_again_errors <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
#       query_args <- tc(list(db = "taxonomy", ID = ID, api_key = key))
#       tt <- repeat_until_it_works(try_again_errors, "esummary",
#                                   query_args, ...)
#       ttp <- xml2::read_xml(tt)
#       df <- parse_ncbi(ttp)
#       rownames(df) <- 1:nrow(df)
#
#       if (!is.null(division_filter) || !is.null(rank_filter)) {
#         df <- filt(df, "division", division_filter)
#         df <- filt(df, "rank", rank_filter)
#       }
#
#       df <- sub_rows(df, rows)
#       uid <- df$uid
#       if (length(uid) == 1) {
#         direct <- TRUE
#         att <- "found"
#       }
#       if (length(uid) == 0) {
#         uid <- NA_character_
#       }
#
#       if (length(uid) > 1) {
#         # check for exact match
#         matchtmp <- df[
#           tolower(
#             as.character(df$scientificname)) %in% tolower(sciname), "uid"]
#         if (length(matchtmp) == 1) {
#           uid <- as.character(matchtmp)
#           direct <- TRUE
#         }
#       }
#
#       if (length(uid) > 1) {
#         if (!ask) {
#           if (length(uid) == 1) {
#             att <- "found"
#           } else {
#             warning(
#               sprintf(m_more_than_one_found, "UID", sciname),
#               call. = FALSE
#             )
#             uid <- NA_character_
#             att <- m_na_ask_false
#           }
#         } else {
#           # prompt
#           rownames(df) <- 1:nrow(df)
#           message("\n\n")
#           message("\nMore than one UID found for taxon '", sciname, "'!\n
#             Enter rownumber of taxon (other inputs will return 'NA'):\n")
#           print(df)
#           take <- scan(n = 1, quiet = TRUE, what = 'raw')
#
#           if (length(take) == 0) {
#             take <- "notake"
#             att <- "nothing chosen"
#           }
#           if (take %in% seq_len(nrow(df))) {
#             take <- as.numeric(take)
#             message("Input accepted, took UID '",
#                     as.character(df$uid[take]), "'.\n")
#             uid <- as.character(df$uid[take])
#             att <- 'found'
#           } else {
#             uid <- NA_character_
#             att <- "NA due to user input out of range"
#             mssg(messages, "\nReturned 'NA'!\n\n")
#           }
#         }
#       }
#     }
#     return(data.frame(uid, att, multiple = mm, direct = direct,
#                       stringsAsFactors = FALSE))
#   }
#   sciname <- as.character(sciname)
#   outd <- ldply(sciname, fun, ask, messages, rows, ...)
#   out <- structure(outd$uid, class = "uid",
#                    match = outd$att,
#                    multiple_matches = outd$multiple,
#                    pattern_match = outd$direct)
#   add_uri(out, 'https://www.ncbi.nlm.nih.gov/taxonomy/%s')
# }
