# ------------------------------------------------------------------------------
# FINDING EMBEDDING COORDINATES
# ------------------------------------------------------------------------------

# Purpose: find embedding coordinates (unsupervised case)

find_embedding_coordinates <- function(reconstruction_weights, intrinsic_dim) {
  
  # COMPUTE EMBEDDING MATRIX ---------------------------------------------------
  
  n <- nrow(reconstruction_weights)
  embedding_matrix <- crossprod(diag(1L, n) - reconstruction_weights)
  
  # DIAGONALIZE EMBEDDING MATRIX -----------------------------------------------
  
  eigenanalysis <- eigen(embedding_matrix, symmetric = TRUE)
  
  # Find bottom intrinsic_dim + 1 eigenvectors 
  
  # TODO check whether automatic selection of d works
  # see de ridder p. 7
  
  idx_bottom_eigenvectors <- seq(n - intrinsic_dim, n - 1L, by = 1L)
    
  # COMPUTE EMBEDDING COORDINATES ----------------------------------------------
  
  embedding_coordinates <- rev(as.data.table(
      eigenanalysis$vectors[, idx_bottom_eigenvectors] * sqrt(n)))
  data.table::setnames(
    embedding_coordinates, 
    sprintf("y_%d", seq_len(intrinsic_dim)))
  
  # RETURN ---------------------------------------------------------------------
  
  embedding_coordinates
  
}
