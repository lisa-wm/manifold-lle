# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_reconstruction_weights <- function(data, 
                                           neighborhood_method, 
                                           neighborhood_size, 
                                           intrinsic_dim) {
  
  # COMPUTE NEIGHBORHOOD MATRIX ------------------------------------------------
  
  cat("finding neighbors...\n")
  
  neighborhood_matrix <- find_neighbors(
    data,
    neighborhood_method,
    neighborhood_size
  )
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------
  
  # TODO read up on regularization methods for inverting gram matrix

  cat("computing reconstruction weights...\n")
  
  weight_matrix <- t(sapply(
    
    data[, .I],
    function(i) {

      # Define dimensions
      
      n <- nrow(data)
      p <- ncol(data)
      k <- sum(neighborhood_matrix[i, ])
      
      # Center data wrt i-th observation
      
      data_centered <- sweep(data, 2L, c(t(data[i, ])))
      
      # Filter for neighbors of i-th observation
      
      data_nn <- as.matrix(
        data_centered[neighborhood_matrix[i, ], ], 
        ncol = p)
      
      # Compute gram matrix
      
      gram <- tcrossprod(data_nn)
      
      # Apply regularization
      # TODO find out about this delta and regu
      
      if (k > p) {
        
        delta <- 0.1
        regularization_param <- delta^2 / k * sum(diag(gram))
        gram <- gram + regularization_param * diag(1, k)
        
      }
      
      # Reconstruct weights from les
      
      weights <- t(MASS::ginv(gram)) %*% rep(1, k)
      
      # Enforce sum-1 constraint
      
      weights_normed <- weights / sum(weights)
      
      # Pad with zeroes for weight matrix
      
      weights_row <- rep(0, n)
      weights_row[neighborhood_matrix[i, ]] <- weights_normed
      
      t(weights_row)
      
    }
  ))
  
  # MAKE FINAL CHECK -----------------------------------------------------------
  
  if (any(round(apply(weight_matrix, 1, sum)) != 1L)) {
    stop("something went wrong during weight computation")
  }
  
  # RETURN ---------------------------------------------------------------------

  weight_matrix
  
}
