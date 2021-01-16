# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: WEIGHT COMPUTATION
# ------------------------------------------------------------------------------

compute_weights <- function(data, neighborhood_matrix) {
  
  i <- 1L
  
  x_i <- unlist(data[i, ])
  
  neighborhood_data_i <- as.matrix(data[neighborhood_matrix[i, ], ])
  
  loc_cov <- matrix(
    c(t(data)) - c(t(data[i, ])),
    nrow = nrow(data),
    byrow = TRUE)
  
  loc_cov_2 <- matrix(
    loc_cov[neighborhood_matrix[i, ]], 
    ncol = ncol(data),
    nrow = neighborhood_size)
  
  gram <- tcrossprod(loc_cov_2, loc_cov_2)

  
}
