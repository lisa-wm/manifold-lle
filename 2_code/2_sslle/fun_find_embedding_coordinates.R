# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: FINDING EMBEDDING COORDINATES
# ------------------------------------------------------------------------------

find_embedding_coordinates <- function(reconstruction_weights, intrinsic_dim) {
  
  # compute embedding matrix
  
  n <- nrow(reconstruction_weights)
  embedding_matrix <- crossprod(diag(1L, n) - reconstruction_weights)
  
  # find bottom d + 1 eigenvectors
  
  eigenanalysis <- eigen(embedding_matrix, symmetric = TRUE)
  idx_bottom_eigenvectors <- seq(n - intrinsic_dim, n - 1L, by = 1L)
    
  # compute low-dimensional coordinates
  
  embedding_coordinates <- as.data.table(
    eigenanalysis$vectors[, idx_bottom_eigenvectors] * sqrt(n))
  setnames(embedding_coordinates, sprintf("y_%d", seq_len(intrinsic_dim)))
  
  embedding_coordinates
  
}
