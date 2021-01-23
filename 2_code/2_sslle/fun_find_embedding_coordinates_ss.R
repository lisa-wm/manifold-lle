# ------------------------------------------------------------------------------
# FINDING EMBEDDING COORDINATES SEMI-SUPERVISEDLY
# ------------------------------------------------------------------------------

# Purpose: find embedding coordinates (semi-supervised case)

find_embedding_coordinates_ss <- function(reconstruction_weights, 
                                          prior_points,
                                          is_exact,
                                          confidence_param) {
  
  # COMPUTE EMBEDDING MATRIX ---------------------------------------------------
  
  # Define dimensions
  
  n <- nrow(reconstruction_weights)
  m <- nrow(prior_points)
  d <- ncol(prior_points)
  
  embedding_matrix <- crossprod(diag(1L, n) - reconstruction_weights)
  
  # BLOCK-DIVIDE EMBEDDING MATRIX ----------------------------------------------
  
  # Decompose into block matrices (top left part corresponding to prior points)
  
  embedding_matrix_11 <- embedding_matrix[1:m, 1:m]
  embedding_matrix_12 <- embedding_matrix[(m + 1):n, 1:m]
  embedding_matrix_22 <- embedding_matrix[(m + 1):n, (m + 1):n]
  
  # SOLVE LES ------------------------------------------------------------------
  
  if (is_exact) {
    
    embedding_matrix_22_inv <- MASS::ginv(embedding_matrix_22)
    
    embedding_coordinates <- as.data.table(
      embedding_matrix_22_inv %*% embedding_matrix_12 %*% 
        as.matrix(prior_points))
    
    data.table::setnames(prior_points, names(embedding_coordinates))
    
    embedding_coordinates <- rbind(
      prior_points,
      embedding_coordinates
    )
    
  } else {
    
    embedding_matrix_penalized <- matrix(
      rbind(
        cbind(
          embedding_matrix_11 + diag(confidence_param, m),
          t(embedding_matrix_12)), 
        cbind(
          embedding_matrix_12,
          embedding_matrix_22)),
      ncol = n)
    
    embedding_coordinates <- solve(
      embedding_matrix_penalized,
      as.matrix(rbind(
        confidence_param * prior_points,
        matrix(0, nrow = n - m, ncol = d),
        use.names = FALSE)))
    
    embedding_coordinates <- as.data.table(embedding_coordinates)
    
  }
  
  # data.table::setnames(
  #   embedding_coordinates, 
  #   sprintf("y_%d", seq_len(d)))
  
  # RETURN ---------------------------------------------------------------------
  
  embedding_coordinates
  
}
