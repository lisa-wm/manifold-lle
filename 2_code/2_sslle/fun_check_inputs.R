# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: INPUT CHECKS
# ------------------------------------------------------------------------------

# TODO Make better if there is time

check_inputs <- function(data,
                         intrinsic_dim, 
                         neighborhood_method, 
                         neighborhood_size) {
  
  checkmate::assert_count(intrinsic_dim)
  
  if (ncol(data) <= intrinsic_dim) {
    stop("intrinsic dimensionality must be lower than input dimensionality")
  }
  
  neighborhood_method <- match.arg(neighborhood_method, c("knn", "epsilon"))
  
  checkmate::assert_numeric(
    neighborhood_size, 
    lower = 1e-08,
    finite = TRUE)
  
  if (neighborhood_method == "knn" & 
      !checkmate::test_count(neighborhood_size)) {
    stop("for k-neighborhoods, neighborhood size must be a positive integer")
  }
  
  if (neighborhood_method == "knn" & neighborhood_size <= intrinsic_dim) {
    stop("neighborhood size must be greater than intrinsic dimension")
  }
  
  if (neighborhood_method == "epsilon" & min(dist(data)) > neighborhood_size) {
    stop("neighborhood size too small, no neighbors found")
  }
  
  if (neighborhood_method == "epsilon" & max(dist(data)) < neighborhood_size) {
    stop("neighborhood size too large, single neighborhood produced")
  }
  
}
