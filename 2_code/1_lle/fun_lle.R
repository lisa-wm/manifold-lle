# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: MAIN FUNCION
# ------------------------------------------------------------------------------

compute_lle <- function(data,
                        intrinsic_dim = 2L,
                        is_knn = TRUE,
                        neighborhood_size) {
  
  # Perform input checks and harmonization
  
  check_inputs(data, intrinsic_dim, is_knn, neighborhood_size)
  data <- as.data.table(data)
  
  # Find nearest neighbors
  
  neighborhood_matrix <- find_neighbors(data, is_knn, neighborhood_size)
  
  # Compute reconstruction weights in input space
  
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
