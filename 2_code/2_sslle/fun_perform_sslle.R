# ------------------------------------------------------------------------------
# PERFORMING SSLLE
# ------------------------------------------------------------------------------

# Purpose: perform sslle

perform_sslle <- function(data,
                          prior_points,
                          neighborhood_method = c("knn", "epsilon"),
                          neighborhood_size,
                          landmark = FALSE) {
  
  # TODO do regularization properly
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  invisible(sapply(list(data, prior_points), function(i) {
    check_data(i)
    assign(deparse(substitute(i)), as.data.table(i))
    }))
  
  # Check argument validity
  
  if (nrow(data) <= nrow(prior_points)) {
    stop("sslle only makes sense if not all points are known yet")
  }
  intrinsic_dim <- ncol(prior_points)
  check_inputs(data, intrinsic_dim, neighborhood_method, neighborhood_size)
  
  # COMPUTE RECONSTRUCTION WEIGHTS ---------------------------------------------
  
  reconstruction_weights <- compute_reconstruction_weights(
    data, 
    neighborhood_method, 
    neighborhood_size,
    intrinsic_dim)
  
  # COMPUTE EMBEDDING COORDINATES ----------------------------------------------
  
  cat("finding embedding coordinates...\n")
  
  embedding_coordinates <- find_embedding_coordinates_ss(
    reconstruction_weights, 
    prior_points
  )
  
  data.table::setnames(prior_points, names(embedding_coordinates))
  
  all_embedding_coordinates <- rbind(
    prior_points,
    embedding_coordinates
  )
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    X = data,
    Y = abs(all_embedding_coordinates)
  )
  
}
