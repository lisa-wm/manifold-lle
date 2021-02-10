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
                          regularization_param = 1e-4) {
  
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
  
  reconstruction_results <- compute_reconstruction_weights(
    data,
    k_max,
    regularization)
  
  # plot(reconstruction_weights$results_search_k$reconstruction_errors ~
  #        reconstruction_weights$results_search_k$neighborhood_sizes, 
  #      type = "l")
  
  # reconstruction_weights <- reconstruction$weight_matrix
  # optimal_k <- reconstruction$neighborhood_size

  # COMPUTE EMBEDDING COORDINATES FOR CANDIDATE NEIGHBORHOOD SIZES -------------
  
  cat(sprintf(
    "finding embedding coordinates for %d candidate neighborhood sizes...\n",
    length(reconstruction_results$candidates_k)))
 
  embedding_results <- lapply(
    reconstruction_results$candidates_k,
    function(i) {
      find_embedding_coordinates_ss(
        reconstruction_results$search_k$weight_matrices[[i]], 
        prior_points,
        is_exact,
        confidence_param)})
    
  # COMPUTE RESIDUAL VARIANCES -------------------------------------------------

  residual_variances <- lapply(
    seq_along(reconstruction_results$candidates_k),
    function(i) {
      1 - cor(
        c(as.matrix(dist(data))), 
        c(as.matrix(dist(embedding_results[[i]]$embedding_coordinates))))})
  
  # COMPUTE AUC_LNK_RNX --------------------------------------------------------
  
  auc_lnk_rnx <- lapply(
    seq_along(reconstruction_results$candidates_k),
    function(i) {
      compute_auc_lnk_rnx(
        data, 
        embedding_results[[i]]$embedding_coordinates)})
  
  # FIND OPTIMAL EMBEDDING -----------------------------------------------------
  
  # embedding_opt <- which.min(residual_variances)
  embedding_opt <- which.max(auc_lnk_rnx)
  
  # RETURN ---------------------------------------------------------------------
  
  list(
    neighborhood_search = list(
      neighborhood_sizes = 
        reconstruction_results$search_k$neighborhood_sizes,
      reconstruction_errors = 
        reconstruction_results$search_k$reconstruction_errors),
    neighborhood_candidates = reconstruction_results$candidates_k,
    neighborhood_size = reconstruction_results$candidates_k[embedding_opt],
    X = data,
    Y = abs(embedding_results[[embedding_opt]]$embedding_coordinates))
  
}
