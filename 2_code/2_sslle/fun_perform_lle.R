# ------------------------------------------------------------------------------
# PERFORMING LLE
# ------------------------------------------------------------------------------

# Purpose: perform lle

perform_lle <- function(data,
                        intrinsic_dim = 2L,
                        neighborhood_method = c("knn", "epsilon"),
                        choices_k,
                        regularization = TRUE,
                        regularization_param = 1e-4,
                        landmark = FALSE) {
  
  # FIXME do neighborhood search properly
  # TODO adjust input checks
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  check_data(data)
  data <- as.data.table(data)
  
  # Check argument validity
  
  # check_inputs(
  #   data, 
  #   intrinsic_dim, 
  #   neighborhood_method, 
  #   neighborhood_size,
  #   regularization_param)
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------

  reconstruction <- compute_reconstruction_weights(
    data, 
    neighborhood_method, 
    choices_k,
    regularization,
    regularization_param)
  
  reconstruction_weights <- reconstruction$weight_matrix
  optimal_k <- reconstruction$neighborhood_size
  
  cat(sprintf("chose %d neighbors\n", optimal_k))
  
  # COMPUTE EMBEDDING COORDINATES ----------------------------------------------
  
  cat("finding embedding coordinates...\n")
  
  embedding_coordinates <- find_embedding_coordinates(
    reconstruction_weights, 
    intrinsic_dim
  )
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    X = data,
    k = optimal_k,
    Y = embedding_coordinates
  )
  
}
