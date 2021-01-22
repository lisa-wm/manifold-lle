# ------------------------------------------------------------------------------
# DATA CHECKS
# ------------------------------------------------------------------------------

# Purpose: perform input checks on data

check_data <- function(data) {
  
  # Data class
  
  if (!any(
    checkmate::test_matrix(data),
    checkmate::test_data_frame(data),
    checkmate::test_data_table(data))) {
    
    stop("data must be of class matrix, data.frame or data.table")
    
  }
  
  # Check for feature dimensionality
  
  if (ncol(data) < 2) {
    stop("dimensionality reduction only makes sense with at least two features")
  }
  
  # Check whether inputs are numeric
  
  if (!checkmate::test_numeric(unlist(data))) {
    stop("features must be numeric to perform lle")
  }
  
  # Check for missing values
  
  if (any(is.na(data))) {
    stop("data may not have missing values")
  }
  
}
