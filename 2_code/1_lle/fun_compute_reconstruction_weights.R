# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_reconstruction_weights <- function(data, 
                                           neighborhood_matrix, 
                                           intrinsic_dim) {
  
  # TODO read up on regularization methods for inverting gram matrix
  
  # weight_matrix_init <- diag(0, nrow = nrow(data))
  
  weight_matrix <- t(sapply(
    
    data[, .I],
    function(i) {

      # dimensions
      
      n <- nrow(data)
      p <- ncol(data)
      k <- sum(neighborhood_matrix[i, ])
      
      # center data wrt i-th obs
      
      data_centered <- sweep(data, 2L, c(t(data[i, ])))
      
      # filter for neighbors of i-th obs
      
      data_nn <- as.matrix(
        data_centered[neighborhood_matrix[i, ], ], 
        ncol = p)
      
      # compute local covariance matrix
      
      gram <- tcrossprod(data_nn)
      
      # apply regularization
      # TODO find out about this delta and regu
      
      if (k > p) {
        
        delta <- 0.1
        regularization_param <- delta^2 / k * sum(diag(gram))
        gram <- gram + regularization_param * diag(1, k)
        
      }
      
      # reconstruct weights from les
      
      weights <- t(MASS::ginv(gram)) %*% rep(1, k)
      
      # enforce sum-1 constraint
      
      weights_normed <- weights / sum(weights)
      
      # pad with zeroes for weight matrix
      
      weights_row <- rep(0, n)
      weights_row[neighborhood_matrix[i, ]] <- weights_normed
      
      t(weights_row)
      
    }
  ))

}
