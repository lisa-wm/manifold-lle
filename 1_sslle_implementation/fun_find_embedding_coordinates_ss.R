# ------------------------------------------------------------------------------
# FINDING EMBEDDING COORDINATES SEMI-SUPERVISEDLY
# ------------------------------------------------------------------------------

# Purpose: compute embedding coordinates in SSLLE implementation

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
  
  embedding_matrix_11 <- embedding_matrix[seq_len(m), seq_len(m)]
  embedding_matrix_12 <- embedding_matrix[(m + 1L):n, seq_len(m)]
  embedding_matrix_22 <- embedding_matrix[(m + 1L):n, (m + 1):n]
  
  # SOLVE LES ------------------------------------------------------------------
  
  if (is_exact) {
    
    embedding_matrix_22_inv <- MASS::ginv(embedding_matrix_22)
    
    embedding_coordinates <- data.table::as.data.table(
      embedding_matrix_22_inv %*% embedding_matrix_12 %*% 
        as.matrix(prior_points))
    
    data.table::setnames(prior_points, sprintf("y_%d", seq_len(d)))
    data.table::setnames(embedding_coordinates, sprintf("y_%d", seq_len(d)))
    
    embedding_coordinates <- rbind(
      prior_points,
      embedding_coordinates)
    
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
    
    # Try to invert penalized embedding matrix
    
    embedding_coordinates <- try(
      solve(
        embedding_matrix_penalized,
        as.matrix(rbind(
          confidence_param * prior_points,
          matrix(0L, nrow = n - m, ncol = d),
          use.names = FALSE))),
      silent = TRUE)
    
    # If penalized embedding matrix is singular, apply small regularization 
    # constant
    
    if (inherits(embedding_coordinates, "try-error")) {
      
      embedding_matrix_penalized <- embedding_matrix_penalized +
        diag(1e-4, nrow = nrow(embedding_matrix_penalized))
      
      embedding_coordinates <- try(
        solve(
          embedding_matrix_penalized,
          as.matrix(rbind(
            confidence_param * prior_points,
            matrix(0L, nrow = n - m, ncol = d),
            use.names = FALSE))))
      
    }
    
    embedding_coordinates <- data.table::as.data.table(embedding_coordinates)
    data.table::setnames(embedding_coordinates, sprintf("y_%d", seq_len(d)))
    
  }
  
  embedding_distances <- as.matrix(dist(embedding_coordinates))
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    embedding_coordinates = embedding_coordinates,
    embedding_distances = embedding_distances)
  
}
