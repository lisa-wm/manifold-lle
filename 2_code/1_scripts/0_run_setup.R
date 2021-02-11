# ------------------------------------------------------------------------------
# SET-UP
# ------------------------------------------------------------------------------

# Purpose: install, if necessary, and load packages, source required files

# In:  -
# Out: -

# INSTALL AND LOAD PACKAGES ----------------------------------------------------

packages_required <-  c(
  
  "checkmate", # input checking
  "coRanking", # dimensionality reduction evaluation
  "data.table", # data wrangling
  "dimRed", # dimensionality reduction, in particluar, isomap
  "here", # path management
  "igraph", # graph visualizations
  "kknn", # k-nearest neighbor search
  "MASS", # matrix computation
  "mlbench", # artificial data sets for benchmarking
  "plotly", # 3d plots
  "tidyverse" # data wrangling

)

set_up_packages <- function(pkg) {
  
  my_type <- ifelse(
    Sys.info()[["sysname"]] == "Linux", 
    "source", 
    "binary"
  )
  
  not_installed <- 
    packages_required[!packages_required %in% installed.packages()[, "Package"]]
  
  if (length(not_installed) > 0) {
    
    lapply(
      not_installed,
      install.packages,
      repos = "http://cran.us.r-project.org",
      dependencies = TRUE,
      type = my_type
    )
    
  }
  
  lapply(packages_required, library, character.only = TRUE, quietly = TRUE)
  
}

invisible(set_up_packages(packages_required))

# SOURCE FILES -----------------------------------------------------------------

# Find and source all files containing functions

files_required <- list.files(
  here("2_code"), 
  pattern = "^fun_.*\\.R$", 
  recursive = TRUE,
  full.names = TRUE)

invisible(sapply(files_required, source, .GlobalEnv))
