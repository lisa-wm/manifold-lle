# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_reconstruction_weights <- function(data, 
                                           neighborhood_method, 
                                           choices_k,
                                           regularization,
                                           regularization_param) {
  
  # COMPUTE NEIGHBORHOOD MATRIX ------------------------------------------------
  
  cat("finding neighbors...\n")
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------
  
  # TODO read up on regularization methods for inverting gram matrix
  
  cat("computing reconstruction weights...\n")
  
  results_k <- lapply(
    
    choices_k,
    
    function(k) {
      
      cat(sprintf("trying %d neighbors...\n", k))
      
      neighborhood_matrix <- find_neighbors(
        data,
        neighborhood_method,
        k)
      
      reconstruction <- lapply(
        
        data[, .I],
        function(i) {
          
          # Define dimensions
          
          n <- nrow(data)
          p <- ncol(data)
          # k <- sum(neighborhood_matrix[i, ])
          
          # Center data with respect to i-th observation
          
          data_centered <- sweep(data, 2L, c(t(data[i, ])))
          
          # Filter for neighbors of i-th observation
          
          data_nn <- as.matrix(
            data_centered[neighborhood_matrix[i, ], ], 
            ncol = p)
          
          # Compute gram matrix
          
          gram <- tcrossprod(data_nn)
          
          # Apply regularization
          # TODO find out about this delta and regu
          
          if (regularization & k > p) {
            
            # roweis saul themselves mention regu, but no details
            # this is taken from grilli diss
            # this is now concept from ghojogh (l2 penalization) with penalization 
            # param from grilli
            # lle pkg does similar thing but sum diagonal is not equal to 
            # eigenvalues, gram is not diagonal
            # delta is arbitrary, seems to work
            
            eigenvalues_gram <- eigen(gram)$values
            gamma <- regularization_param * sum(eigenvalues_gram)
            
            gram <- gram + diag(gamma, k)
            
          }
          
          # Reconstruct weights from les
          
          gram_inv <- MASS::ginv(gram)
          
          weights <- (gram_inv %*% rep(1, k)) / 
            as.numeric(crossprod(rep(1, k), gram_inv) %*% rep(1, k))
          
          # Enforce sum-1 constraint
          
          weights_normed <- weights / sum(weights)
          
          # Pad with zeroes for weight matrix
          
          weights_row <- rep(0, n)
          weights_row[neighborhood_matrix[i, ]] <- weights_normed
          
          err <- crossprod(weights_normed, gram) %*% weights_normed
          
          list(weights_row, err)
          
        }
      )
      
      weight_matrix <- do.call(rbind, do.call(rbind, reconstruction)[, 1])
      reconstruction_err <- sum(unlist(do.call(rbind, reconstruction)[, 2]))
      
      list(k, weight_matrix, reconstruction_err)
      
    }
    
  )
  
  neighborhood_sizes <- unlist(do.call(rbind, results_k)[, 1])
  weight_matrices <- do.call(rbind, results_k)[, 2]
  reconstruction_errors <- unlist(do.call(rbind, results_k)[, 3])
  
  optimal_k_ind <- which.min(reconstruction_errors)
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    weight_matrix = do.call(rbind, weight_matrices[optimal_k_ind]),
    neighborhood_size = neighborhood_sizes[optimal_k_ind])
  
}
