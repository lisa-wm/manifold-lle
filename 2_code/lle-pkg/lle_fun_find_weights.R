# ------------------------------------------------------------------------------
# PACKAGE LLE: FUNCTION COMPUTING RECONSTRUCTION WEIGHTS
# ------------------------------------------------------------------------------

lle_find_weights <- function (nns, X, m, reg = 2, ss = FALSE, p = 0.5, 
                              id = FALSE, 
                          v = 0.99) 
{
  N <- dim(X)[1]
  n <- dim(X)[2]
  
  # Initialize weights with zero
  
  wgts <- 0 * matrix(0, N, N)
  s <- c()
  intr_dim <- c()
  
  for (i in (1:N)) {
    
    # Compute k (why not take as argument?)
    
    k <- sum(nns[i, ])
    if (k == 0) 
      next
    
    # Compute feature-wise difference to i-th observation
    
    Z <- matrix(c(t(X)) - c(t(X[i, ])), nrow = nrow(X), byrow = TRUE)
    
    
    Z <- matrix(Z[nns[i, ], ], ncol = n, nrow = k)
    G <- Z %*% t(Z)
    delta <- 0.1
    e <- eigen(G, symmetric = TRUE, only.values = TRUE)$values
    if (all(e == 0)) 
      next
    
    # Regu var 
    
    if (reg == 1) {
      r <- delta * sum(head(e, n - m))/(n - m)
    } else if (reg == 2) {
      r <- delta^2/k * sum(diag(G))
    } else r <- 3 * 10^-3
    
    if (id) {
      tmp <- 1
      while (sum(e[1:tmp])/sum(e) <= v) tmp <- tmp + 1
      intr_dim <- c(intr_dim, tmp)
    }
    s <- c(s, sum(head(e, n - m))/(n - m))
    if (k > n) {
      alpha <- r
      } else alpha <- 0
    G <- G + alpha * diag(1, k)
    if (k >= 2) 
      wgts[i, nns[i, ]] <- t(ginv(G) %*% rep(1, k))
    else wgts[i] <- G
    wgts[i, ] <- wgts[i, ]/sum(wgts[i, ])
  }
  if (ss) {
    s <- s/sum(s)
    Fs <- ecdf(s)
    choise <- sample(1:N, round(p * N), replace = FALSE, 
                     prob = Fs(seq(0, max(s), length = N)))
    X <- X[choise, ]
  }
  else choise <- 0
  if (id) 
    cat("intrinsic dim: mean=", mean(intr_dim), ", mode=", 
        names(sort(-table(intr_dim)))[1], "\n", sep = "")
  return(list(X = X, wgts = wgts, choise = choise, id = intr_dim))
}
