# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: INPUT CHECKS
# ------------------------------------------------------------------------------

# TODO Make better if there is time

check_inputs <- function(data, intrinsic_dim, is_knn, neighborhood_size) {
  
  # ARGUMENT VALIDITY ----------------------------------------------------------
  
  # Argument data
  
  if (!any(
    checkmate::test_matrix(data),
    checkmate::test_data_frame(data),
    checkmate::test_data_table(data))) {
    
    stop("data must be of class matrix, data.frame or data.table")
    
  }
  
  # Argument intrinsic_dim
  
  if(!checkmate::test_number(intrinsic_dim)) {
    stop("intrinsic_dim must be positive integer")
  }
  
  # Argument is_knn
  
  if(!checkmate::test_logical(is_knn)) {
    stop("is_knn must be either TRUE or FALSE")
  }
  
  # Argument neighborhood_size
  
  if(!checkmate::test_numeric(
    neighborhood_size, 
    lower = 1e-08, 
    finite = TRUE))
    {
    
    stop("neighborhood_size must be positive")
    
  }
  
  # INPUT DATA -----------------------------------------------------------------
  
  # Check for feature dimensionality
  
  if (ncol(data) < 2) {
    stop("lle only makes sense if data have at least two features")
  }
  
  # Check whether inputs are numeric
  
  if (!checkmate::test_numeric(unlist(data))) {
    stop("features must be numeric to perform lle")
  }
  
  # Check for missing values
  
  if (any(is.na(data))) {
    stop("data may not have missing values")
  }
  
  # INTRINSIC DIMENSIONALITY ---------------------------------------------------

  if (ncol(data) <= intrinsic_dim) {
    stop("intrinsic dimensionality must be lower than input dimensionality")
  }
  
  # NEIGHBORHOOD SIZE ----------------------------------------------------------
  
  if (is_knn & !checkmate::test_number(neighborhood_size)) {
    stop("for k-neighborhoods, neighborhood size must be positive integer")
  }
  
  if (!is_knn & min(dist(data)) > neighborhood_size) {
    stop("neighborhood size too small, no neighbors to be found")
  }
  
  if (!is_knn & max(dist(data)) < neighborhood_size) {
    stop("neighborhood size too large, single neighborhood produced")
  }
  
}
