# ------------------------------------------------------------------------------
# PERFORMING SSLLE
# ------------------------------------------------------------------------------

# Purpose: perform sslle

perform_sslle <- function(data,
                          k_max,
                          prior_points,
                          is_exact = TRUE,
                          confidence_param = NULL,
                          regularization = TRUE,
                          regularization_param = 1e-4,
                          verbose = TRUE) {
  
  # TODO adjust input checks
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  # invisible(sapply(list(data, prior_points), function(i) {
  #   check_data(i)
  #   assign(deparse(substitute(i)), as.data.table(i))
  #   }))
  
  # Check argument validity
  
  # if (nrow(data) <= nrow(prior_points)) {
  #   stop("sslle only makes sense if not all points are known yet")
  # }
  # 
  # intrinsic_dim <- ncol(prior_points)
  
  # check_inputs(
  #   data, 
  #   intrinsic_dim, 
  #   neighborhood_method, 
  #   neighborhood_size,
  #   regularization_param)
  
  # if (!is_exact & is.null(confidence_param)) {
  #   stop("please specify a confidence parameter")
  # }
  # 
  # if (!is_exact & !checkmate::test_numeric(
  #   confidence_param, 
  #   lower = 1e-08,
  #   finite = TRUE)) {
  #   stop("confidence parameter must be positive numeric value")
  # }
  
  # COMPUTE RECONSTRUCTION WEIGHTS FOR CANDIDATE NEIGHBORHOOD SIZES ------------
  
  reconstruction <- compute_reconstruction_weights(
    data,
    k_max,
    regularization,
    verbose)
  
  # plot(reconstruction_weights$results_search_k$reconstruction_errors ~
  #        reconstruction_weights$results_search_k$neighborhood_sizes, 
  #      type = "l")
  
  # reconstruction_weights <- reconstruction$weight_matrix
  # optimal_k <- reconstruction$neighborhood_size

  # COMPUTE EMBEDDING COORDINATES FOR CANDIDATE NEIGHBORHOOD SIZES -------------
  
  if (verbose) {
   
    cat(sprintf(
      "finding embedding coordinates for %d candidate neighborhood sizes...\n",
      length(reconstruction$candidates_k)))
     
  }
 
  embedding <- lapply(
    reconstruction$candidates_k,
    function(i) {
      find_embedding_coordinates_ss(
        reconstruction$search_k$weight_matrices[[i]], 
        prior_points,
        is_exact,
        confidence_param)})
    
  # COMPUTE RESIDUAL VARIANCES -------------------------------------------------

  residual_variances <- lapply(
    seq_along(reconstruction$candidates_k),
    function(i) {
      1 - cor(
        c(as.matrix(dist(data))), 
        c(as.matrix(dist(embedding[[i]]$embedding_coordinates))))})
  
  # COMPUTE AUC_LNK_RNX --------------------------------------------------------
  
  auc_lnk_rnx <- lapply(
    seq_along(reconstruction$candidates_k),
    function(i) {
      compute_auc_lnk_rnx(
        data, 
        embedding[[i]]$embedding_coordinates)})
  
  # FIND OPTIMAL EMBEDDING -----------------------------------------------------
  
  # embedding_opt <- which.min(residual_variances)
  embedding_opt <- which.max(auc_lnk_rnx)
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    reconstruction_errors = reconstruction$search_k$reconstruction_errors,
    neighborhood_candidates = reconstruction$candidates_k,
    neighborhood_size = reconstruction$candidates_k[embedding_opt],
    auc_lnk_rnx = unlist(auc_lnk_rnx),
    X = data,
    Y = abs(embedding[[embedding_opt]]$embedding_coordinates))
  
}
