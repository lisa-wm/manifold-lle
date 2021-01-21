# ------------------------------------------------------------------------------
# EXAMPLE TO TEST LLE FUNCTIONS
# ------------------------------------------------------------------------------

# EXAMPLE DATA -----------------------------------------------------------------

data(mtcars)
data <- as.data.table(mtcars[2:10, 1:3])
intrinsic_dim <- 2L
is_knn <- c(TRUE, FALSE)
neighborhood_size <- c(3L, 80L)

# TEST RUNS --------------------------------------------------------------------

# TODO write proper tests

check_inputs(data, intrinsic_dim, is_knn[1], neighborhood_size[1])
check_inputs(data, intrinsic_dim, is_knn[2], neighborhood_size[2])

my_neighbors_knn <- find_neighbors(data, is_knn[1], neighborhood_size[1])
my_neighbors_eps <- find_neighbors(data, is_knn[2], neighborhood_size[2])

weights <- 0 * matrix(0, nrow(data), nrow(data))

for (i in seq_len(nrow(data))) {
  
  z_mat <- matrix(
    c(t(data)) - c(t(data[i, ])), 
    nrow = nrow(data), 
    byrow = TRUE)
  
  nns <- find_neighbors(data, is_knn[1], neighborhood_size[1])
  
  z_mat_small <- matrix(
    z_mat[nns[i, ], ], 
    ncol = ncol(x_mat), 
    nrow = neighborhood_size)
  
  c_mat <- tcrossprod(z_mat_small)
  
  weights[i, nns[i, ]] <- t(MASS::ginv(c_mat)) %*% rep(1, neighborhood_size[1])
  weights[i, ] <- weights[i, ] / sum(weights[i, ])
  
  weights
  
}

