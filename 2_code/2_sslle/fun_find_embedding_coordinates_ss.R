# ------------------------------------------------------------------------------
# FINDING EMBEDDING COORDINATES SEMI-SUPERVISEDLY
# ------------------------------------------------------------------------------

# Purpose: find embedding coordinates (semi-supervised case)

find_embedding_coordinates_ss <- function(reconstruction_weights, 
                                          prior_points) {
  
  # COMPUTE EMBEDDING MATRIX ---------------------------------------------------
  
  n <- nrow(reconstruction_weights)
  embedding_matrix <- crossprod(diag(1L, n) - reconstruction_weights)
  
  # BLOCK-DIVIDE EMBEDDING MATRIX ----------------------------------------------
  
  # Decompose into block matrices (top left part corresponding to prior points)
  
  m <- nrow(prior_points)
  
  embedding_matrix_12 <- embedding_matrix[(m + 1):n, 1:m]
  embedding_matrix_22 <- embedding_matrix[(m + 1):n, (m + 1):n]
  
  # SOLVE LES ------------------------------------------------------------------
  
  intrinsic_dim <- ncol(prior_points)
  
  embedding_coordinates <- as.data.table(
    embedding_matrix_22 %*% embedding_matrix_12 %*% as.matrix(prior_points))

  setnames(embedding_coordinates, sprintf("y_%d", seq_len(intrinsic_dim)))
  
  # RETURN ---------------------------------------------------------------------
  
  embedding_coordinates
  
}
