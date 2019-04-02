original_dir <- getwd()
setwd("data-raw/webservices_table/")
source("R/packages.R")  # loads packages
source("R/functions.R") # defines the functions
source("R/plan.R")      # creates the drake plan
make(plan_webservice_table, # defined in R/plan.R
  verbose = 2
)
setwd(original_dir)
