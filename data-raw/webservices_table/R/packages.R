reqins_pkg <- function(pkg){
  x <- require(pkg, character.only = TRUE, quietly = TRUE)
  if(!x){
    install.packages(pkg)
    require(pkg, character.only = TRUE, quietly = TRUE)
  }
}
# devtools::install_github(repo = "haozhu233/kableExtra")
pkgs <- c("drake", "knitr", "kableExtra", "magrittr", "magick", "dplyr")
x <- lapply(pkgs, reqins_pkg)
if(!all(unlist(lapply(x, is.null)))){
  print("Some packages could not be loaded")
}
