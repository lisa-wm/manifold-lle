# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: MAIN FUNCION
# ------------------------------------------------------------------------------

compute_lle <- function(data,
                        intrinsic_dim = 2L,
                        neighborhood_method = c("knn", "epsilon"),
                        neighborhood_size,
                        landmark = FALSE) {
  
  # TODO do regularization properly
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  check_data(data)
  data <- as.data.table(data)
  
  # Check argument validity
  
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
  
  # FIND NEIGHBORS -------------------------------------------------------------

  cat("finding neighbors...\n")
  
  neighborhood_matrix <- find_neighbors(
    data, 
    neighborhood_method, 
    neighborhood_size)
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------

  cat("computing reconstruction weights...\n")
  
  reconstruction_weights <- compute_reconstruction_weights(
    data, 
    neighborhood_matrix,
    intrinsic_dim)
  
  if (!all.equal(
    apply(reconstruction_weights, 1, sum), 
    rep(1, nrow(reconstruction_weights)))) {
    stop("something went wrong during weight computation")
  }
  
  # Compute coordinates in embedding space
  
  cat("finding embedding coordinates...\n")
  
  embedding_coordinates <- find_embedding_coordinates(
    reconstruction_weights, 
    intrinsic_dim
  )
  
  # Return output
  
  list(
    X = data,
    Y = embedding_coordinates
  )
  
}
