# ------------------------------------------------------------------------------
# PERFORMING SSLLE
# ------------------------------------------------------------------------------

# Purpose: embed data with SSLLE

perform_sslle <- function(data,
                          k_max,
                          prior_points,
                          is_exact = TRUE,
                          confidence_param = NULL,
                          regularization = TRUE,
                          regularization_param = 1e-4,
                          verbose = TRUE) {

  # CHECK INPUTS ---------------------------------------------------------------
  
  # Check data
  
  check_inputs(
    data = data,
    k_max = k_max,
    prior_points = prior_points,
    is_exact = is_exact,
    confidence_param = confidence_param,
    regularization = regularization,
    regularization_param = regularization_param,
    verbose = verbose)
  
  # COMPUTE RECONSTRUCTION WEIGHTS FOR CANDIDATE NEIGHBORHOOD SIZES ------------
  
  reconstruction <- compute_reconstruction_weights(
    data = data,
    k_max = k_max,
    regularization = regularization,
    verbose = verbose)

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
        reconstruction_weights = reconstruction$search_k$weight_matrices[[i]], 
        prior_points = prior_points,
        is_exact = is_exact,
        confidence_param = confidence_param)})
    
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
    residual_variances = unlist(residual_variances),
    auc_lnk_rnx = unlist(auc_lnk_rnx),
    X = data,
    Y = abs(embedding[[embedding_opt]]$embedding_coordinates))
  
}
