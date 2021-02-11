# ------------------------------------------------------------------------------
# PACKAGE LLE: COMPUTATION OF LOW-DIMENSIONAL COORDINATES
# ------------------------------------------------------------------------------

lle_find_coords <- function (wgts, nns, N, n, m) 
{
  W <- wgts
  M <- t(diag(1, N) - W) %*% (diag(1, N) - W)
  e <- eigen(M)
  Y <- e$vectors[, c((N - m):(N - 1))] * sqrt(N)
  return(Y)
}