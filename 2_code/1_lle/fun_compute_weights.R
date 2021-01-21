# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_weights <- function(data, neighborhood_matrix) {
  
  # TODO read up on regularization methods for inverting gram matrix
  
  # weight_matrix_init <- diag(0, nrow = nrow(data))
  
  weight_matrix <- t(sapply(
    
    data[, .I],
    function(i) {

      # filter data for neighbors of i-th obs
      
      data_nn <- as.matrix(
        data[neighborhood_matrix[i, ]], 
        ncol = ncol(data))
      
      # center data wrt i-th obs
      
      data_nn_centered <- sweep(data_nn, 2L, c(t(data[i, ])))
      
      # compute local covariance matrix
      
      local_covariance <- tcrossprod(data_nn_centered)
      
      # reconstruct weights from les
      
      weights <- t(MASS::ginv(local_covariance)) %*% 
        rep(1, neighborhood_size[1])
      
      # enforce sum-1 constraint
      
      weights_normed <- weights / sum(weights)
      
      # pad with zeroes for weight matrix
      
      weights_row <- rep(0, nrow(data))
      weights_row[neighborhood_matrix[i, ]] <- weights_normed
      
      t(weights_row)
      
    }
  ))

}