# ------------------------------------------------------------------------------
# PERFORMING LLE
# ------------------------------------------------------------------------------

# Purpose: perform lle

perform_lle <- function(data,
                        k_max,
                        intrinsic_dim = 2L,
                        regularization = TRUE,
                        regularization_param = 1e-4,
                        landmark = FALSE) {
  
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

  reconstruction_results <- compute_reconstruction_weights(
    data, 
    k_max,
    regularization)
  
  # reconstruction_weights <- reconstruction$weight_matrix
  # optimal_k <- reconstruction$neighborhood_size
  
  cat(sprintf(
    "finding embedding coordinates for %d candidate neighborhood sizes...\n",
    length(reconstruction_results$candidates_k)))
  
  # COMPUTE EMBEDDING COORDINATES ----------------------------------------------
  
  embedding_results <- lapply(
    reconstruction_results$candidates_k,
    function(i) {
      find_embedding_coordinates(
        reconstruction_results$search_k$weight_matrices[[i]], 
        intrinsic_dim)})
  
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
    auc_lnk_rnx = auc_lnk_rnx,
    neighborhood_size = 
      reconstruction_results$candidates_k[embedding_opt],
    X = data,
    Y = abs(
      embedding_results[[embedding_opt]]$embedding_coordinates))
  
}
