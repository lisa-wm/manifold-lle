# ------------------------------------------------------------------------------
# PERFORMING SSLLE
# ------------------------------------------------------------------------------

# Purpose: perform sslle

perform_sslle <- function(data,
                          prior_points,
                          is_exact = TRUE,
                          confidence_param = NULL,
                          neighborhood_method = c("knn", "epsilon"),
                          neighborhood_size,
                          regularization = TRUE,
                          regularization_param = 1e-4) {
  
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
  
  check_inputs(
    data, 
    intrinsic_dim, 
    neighborhood_method, 
    neighborhood_size,
    regularization_param)
  
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
  
  reconstruction_weights <- compute_reconstruction_weights(
    data, 
    neighborhood_method, 
    neighborhood_size,
    intrinsic_dim,
    regularization,
    regularization_param)
  
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
