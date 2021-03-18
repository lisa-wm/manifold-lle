# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_reconstruction_weights <- function(data,
                                           k_max,
                                           regularization,
                                           verbose) {
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------
  
  # TODO read up on regularization methods for inverting gram matrix
  
  if (verbose) cat("computing reconstruction weights...\n")
  
  error_per_size <- lapply(
    
    seq_len(k_max),
    
    function(k) {
      
      if (verbose & (k == 1L | k %% 10L == 0)) {
        cat(sprintf("trying %d neighbors...\n", k))}
      
      neighborhood_matrix <- find_neighbors(data, k)
      
      reconstruction <- lapply(
        
        data[, .I],
        
        function(i) {
          
          # Define dimensions
          
          n <- nrow(data)
          p <- ncol(data)
          
          # Center data with respect to i-th observation
          
          data_centered <- sweep(data, 2L, c(t(data[i, ])))
          
          # Filter for neighbors of i-th observation
          
          data_nn <- as.matrix(
            data_centered[neighborhood_matrix[i, ], ], 
            ncol = p)
          
          # Compute gram matrix
          
          gram <- tcrossprod(data_nn)
          
          # Apply regularization
          
          if (regularization & k > p) {
            
            eigenvalues_gram <- eigen(gram)$values
            gamma <- 1e-2 * sum(eigenvalues_gram)
            
            gram <- gram + diag(gamma, k)
            
          }
          
          # Reconstruct weights from les
          
          gram_inv <- MASS::ginv(gram)
          
          weights <- (gram_inv %*% rep(1L, k)) / 
            as.numeric(crossprod(rep(1L, k), gram_inv) %*% rep(1L, k))
          
          # Enforce sum-1 constraint
          
          weights_normed <- weights / sum(weights)
          
          # Pad with zeroes for weight matrix
          
          weights_row <- rep(0L, n)
          weights_row[neighborhood_matrix[i, ]] <- weights_normed
          
          err <- crossprod(weights_normed, gram) %*% weights_normed
          
          list(weights_row, err)
          
        }
      )
      
      weight_matrix <- do.call(rbind, do.call(rbind, reconstruction)[, 1L])
      reconstruction_error <- sum(unlist(do.call(rbind, reconstruction)[, 2L]))
      
      list(k, weight_matrix, reconstruction_error)
      
    }
  )
  
  neighborhood_sizes <- unlist(do.call(rbind, error_per_size)[, 1L])
  weight_matrices <- do.call(rbind, error_per_size)[, 2L]
  reconstruction_errors <- unlist(do.call(rbind, error_per_size)[, 3L])
  
  is_local_minimum <- sapply(
    
    seq_along(neighborhood_sizes),
    
    function(i) {
      
      is_local_minimum <- ifelse(
        i == 1L,
        reconstruction_errors[i] < reconstruction_errors[i + 1L],
        ifelse(
          i == length(neighborhood_sizes),
          reconstruction_errors[i] < reconstruction_errors[i - 1L],
          (reconstruction_errors[i] < reconstruction_errors[i - 1L] &
          reconstruction_errors[i] < reconstruction_errors[i + 1L])))})

  top_errors <- reconstruction_errors < quantile(reconstruction_errors, 0.1)

  candidates_k <- neighborhood_sizes[is_local_minimum | top_errors]

  # RETURN ---------------------------------------------------------------------
  
  list(
    search_k = list(
      neighborhood_sizes = neighborhood_sizes,
      weight_matrices = weight_matrices,
      reconstruction_errors = reconstruction_errors),
    candidates_k = candidates_k)
  
}

