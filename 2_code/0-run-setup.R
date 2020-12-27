# ------------------------------------------------------------------------------
# SET-UP
# ------------------------------------------------------------------------------

# Install, if necessary, and load required packages

packages_required <-  c(
  
  "checkmate", # input checking
  "data.table", # data wrangling
  "here", # path management
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
