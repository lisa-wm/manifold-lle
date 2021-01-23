# ------------------------------------------------------------------------------
# PERFORMING LLE
# ------------------------------------------------------------------------------

# Purpose: perform lle

perform_lle <- function(data,
                        intrinsic_dim = 2L,
                        neighborhood_method = c("knn", "epsilon"),
                        neighborhood_size,
                        regularization = TRUE,
                        regularization_param = 1e-4,
                        landmark = FALSE) {
  
  # TODO do regularization properly
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  check_data(data)
  data <- as.data.table(data)
  
  # Check argument validity
  
  check_inputs(data, intrinsic_dim, neighborhood_method, neighborhood_size)
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------

  reconstruction_weights <- compute_reconstruction_weights(
    data, 
    neighborhood_method, 
    neighborhood_size,
    intrinsic_dim,
    regularization)
  
  # COMPUTE EMBEDDING COORDINATES ----------------------------------------------
  
  cat("finding embedding coordinates...\n")
  
  embedding_coordinates <- find_embedding_coordinates(
    reconstruction_weights, 
    intrinsic_dim
  )
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    X = data,
    Y = embedding_coordinates
  )
  
}
