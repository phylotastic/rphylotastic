plan_webservice_table <- drake_plan(
  strings_in_dots = "literals",
  all_categories = c("Common Names to Scientific Names",
      "Scientific Name Extraction",
      "Taxonomic Name Resolution",
      "Taxon Sampling",
      "Taxon Information and Images",
      "Tree Retrieval",
      "Tree Scaling",
      "Tree Comparison",
      "List Management"
  ),
  all_services = make_allservices(all_categories),
  all_descriptions = make_alldescriptions(all_services),
  table1 = make_table1(all_services, all_descriptions, image = FALSE),
  table1_img = make_table1(all_services, all_descriptions)
#   report = knitr::knit(knitr_in("webservices_table.Rmd"), file_out("webservices_table.md"), quiet = TRUE),
#   summary_pdf_report = render_pdf("webservices_table", "", "try15")
)
