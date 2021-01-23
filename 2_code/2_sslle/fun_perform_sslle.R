# ------------------------------------------------------------------------------
# PERFORMING SSLLE
# ------------------------------------------------------------------------------

# Purpose: perform sslle

perform_sslle <- function(data,
                          prior_points,
                          is_exact = TRUE,
                          confidence_param = NULL,
                          neighborhood_method = c("knn", "epsilon"),
                          choices_k,
                          regularization = TRUE,
                          regularization_param = 1e-4) {
  
  # FIXME do neighborhood search properly
  # TODO adjust input checks
  
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
  
  # check_inputs(
  #   data, 
  #   intrinsic_dim, 
  #   neighborhood_method, 
  #   neighborhood_size,
  #   regularization_param)
  
  if (!is_exact & is.null(confidence_param)) {
    stop("please specify a confidence parameter")
  }
  
  if (!is_exact & !checkmate::test_numeric(
    confidence_param, 
    lower = 1e-08,
    finite = TRUE)) {
    stop("confidence parameter must be positive numeric value")
  }
  
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
  
  embedding_coordinates <- find_embedding_coordinates_ss(
    reconstruction_weights, 
    prior_points,
    is_exact,
    confidence_param
  )
  
  # data.table::setnames(prior_points, names(embedding_coordinates))
  # 
  # all_embedding_coordinates <- rbind(
  #   prior_points,
  #   embedding_coordinates
  # )
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    X = data,
    Y = abs(embedding_coordinates)
  )
  
}
