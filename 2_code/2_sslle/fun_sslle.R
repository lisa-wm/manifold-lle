# ------------------------------------------------------------------------------
# SSLLE IMPLEMENTATION: MAIN FUNCION
# ------------------------------------------------------------------------------

compute_sslle <- function(data_labeled,
                          data_unlabeled,
                          intrinsic_dim = 2L,
                          is_knn = TRUE,
                          nighborhood_size) {
  
  # TODO do regularization properly
  
  # Perform input checks and harmonization
  # TODO adapt to sslle case
  
  check_inputs(data_unlabeled, intrinsic_dim, is_knn, neighborhood_size)
  data <- as.data.table(data)
  
  # Find nearest neighbors
  
  cat("finding neighbors...\n")
  
  neighborhood_matrix <- find_neighbors(data, is_knn, neighborhood_size)
  
  # Compute reconstruction weights in input space
  
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
  
  # Compute embedding matrix
  
  cat("finding embedding coordinates...\n")
  
  n <- nrow(reconstruction_weights)
  embedding_matrix <- crossprod(diag(1L, n) - reconstruction_weights)
  
  # Return output
  
  list(
    X = data,
    Y = embedding_coordinates
  )
  
}
