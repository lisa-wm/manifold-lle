# ------------------------------------------------------------------------------
# PACKAGE LLE: MAIN FUNCTION
# ------------------------------------------------------------------------------

lle_lle <- function (X, m, k, reg = 2, ss = FALSE, p = 0.5, id = FALSE, 
                 nnk = TRUE, eps = 1, iLLE = FALSE, v = 0.99) 
{
  choise <- c()
  
  # Some improvement after Wang
  
  if (iLLE) 
    cat("finding neighbours using iLLE\n")
  else cat("finding neighbours\n")
  
  # Check whether k or epsilon neighborhood and compute
  
  if (nnk) 
    nns <- find_nn_k(X, k, iLLE)
  else nns <- find_nn_eps(X, eps)
  cat("calculating weights\n")
  
  # Find reconstruction weights in input space
  
  res_wgts <- find_weights(nns, X, m, reg, ss, p, id, v)
  
  # Uses only a subset of the data to save computation costs
  
  if (ss) {
    X <- res_wgts$X
    choise <- res_wgts$choise
    cat("finding neighbours again\n")
    if (nnk) 
      nns <- find_nn_k(X, k, iLLE)
    else nns <- find_nn_eps(X, eps)
    cat("calculating weights again\n")
    res_wgts <- find_weights(nns, X, m, reg, FALSE, p, id, 
                             v)
  }
  wgts <- res_wgts$wgts
  id <- res_wgts$id
  cat("computing coordinates\n")
  Y <- find_coords(wgts, nns, N = dim(X)[1], n = dim(X)[2], 
                   m)
  return(list(Y = Y, X = X, choise = choise, id = id))
}