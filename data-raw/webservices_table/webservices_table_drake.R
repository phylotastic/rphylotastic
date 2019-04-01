reqins_pkg <- function(pkg){
  x <- require(pkg, character.only = TRUE, quietly = TRUE)
  if(!x){
    install.packages(pkg)
    require(pkg, character.only = TRUE, quietly = TRUE)
  }
}
devtools::install_github(repo = "haozhu233/kableExtra")
pkgs <- c("drake", "knitr", "kableExtra")
x <- lapply(pkgs, reqins_pkg)
if(!all(unlist(lapply(x, is.null)))){
  print("Some packages could not be loaded")
}
render_pdf <- function(reportname, dir, placeholder) {
  original.dir <- getwd()
  setwd(dir)
  system(paste0('pandoc ', paste0(reportname, '.md'), ' -o ', paste0(reportname, '.pdf --pdf-engine=xelatex -V mainfonts="DejaVu Sans"')))
  setwd(original.dir)
  # pandoc -o emoji.pdf --pdf-engine=lualatex -V mainfonts="DejaVu Sans"
  # pandoc -o emoji.pdf --pdf-engine=xelatex  -V mainfonts="DejaVu Sans"
}
plan_webservice_table <- drake_plan(
  report = knitr::knit(knitr_in("data-raw/web_services_table.Rmd"), file_out("data-raw/web_services_table.md"), quiet = TRUE),
  summary_pdf_report = render_pdf("data-raw/web_services_table.md", "data-raw", report)
)
make(plan_webservice_table)