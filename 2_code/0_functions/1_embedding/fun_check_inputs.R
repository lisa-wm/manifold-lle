# ------------------------------------------------------------------------------
# SSLLE IMPLEMENTATION: INPUT CHECKS
# ------------------------------------------------------------------------------

check_inputs <- function(data,
                         k_max,
                         prior_points,
                         is_exact,
                         confidence_param,
                         regularization,
                         regularization_param,
                         verbose) {
  
  # Check formats
  
  dt <- data.table::as.data.table(data)
  pp <- data.table::as.data.table(prior_points)
  checkmate::assert_count(k_max)
  checkmate::assert_logical(is_exact)
  checkmate::assert_numeric(
    confidence_param, 
    lower = 0L, 
    finite = TRUE,
    len = 1L,
    null.ok = TRUE)
  checkmate::assert_logical(regularization)
  checkmate::assert_numeric(
    regularization_param, 
    lower = 0L, 
    finite = TRUE,
    len = 1L,
    null.ok = TRUE)
  checkmate::assert_logical(verbose)
  
  # Check semantics
  
  intrinsic_d <- ncol(pp)
  capital_d <- ncol(dt) - intrinsic_d

  if (capital_d <= intrinsic_d) {
    stop("intrinsic dimensionality must be lower than input dimensionality")}
  
  if (nrow(pp) >= nrow(dt)) {
    stop("number of prior points must be smaller than number of observations")
  }
  
}
