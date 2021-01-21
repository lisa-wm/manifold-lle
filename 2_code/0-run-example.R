# ------------------------------------------------------------------------------
# EXAMPLE TO TEST LLE FUNCTIONS
# ------------------------------------------------------------------------------

# EXAMPLE DATA -----------------------------------------------------------------

data_sr <- make_swiss_roll(n_points = 1000L)
data_observed <- data_sr[, .(x, y, z)]

# check neighbors

neighbors_them <- lle::find_nn_k(data_observed, 10L)
neighbors_me <- find_neighbors(data_observed, TRUE, 10L)

sum(neighbors_them != neighbors_me) # 0

# check weights

weights_them <- lle::find_weights(neighbors_them, data_observed, 2L)$wgts
weights_me <- compute_reconstruction_weights(data_observed, neighbors_me)

all.equal(weights_them, weights_me)

# check embedding

coords_them <- lle::find_coords(
  weights_them, 
  neighbors_them, 
  nrow(data_observed), 
  3, 
  2)

coords_me <- find_embedding_coordinates(weights_me, 2)

all.equal(coords_them[, 1], coords_me$y_1, tolerance = 1e-5)
all.equal(coords_them[, 2], coords_me$y_2, tolerance = 1e-5)

# check output

result_them <- lle::lle(
  data_observed,
  m = 2L,
  k = 10L,
  reg = 2L)

result_me <- compute_lle(
  data_observed,
  intrinsic_dim = 2L,
  is_knn = TRUE,
  neighborhood_size = 10L)

all.equal(result_them$Y[, 1], result_me$Y$y_1, tolerance = 1e-5)
all.equal(result_them$Y[, 2], result_me$Y$y_2, tolerance = 1e-5)

# check plots

data_plot_them <- as.data.table(
  cbind(result_them$Y[, 2], result_them$Y[, 1], data_sr[, t]))
setnames(data_plot_them, c("x", "y", "t"))
data_plot_me <- as.data.table(
  cbind(result_me$Y$y_2, result_me$Y$y_1, data_sr[, t]))
setnames(data_plot_me, c("x", "y", "t"))

all.equal(data_plot_them, data_plot_me, tolerance = 1e-5)

plot_manifold_2d(data_plot_them)
plot_manifold_2d(data_plot_me)

dim_red_scurve <- compute_lle(
  make_s_curve(n_points = 500L),
  intrinsic_dim = 2L,
  is_knn = TRUE,
  neighborhood_size = 10L
)

data_plot_scurve <- as.data.table(cbind(
  dim_red_scurve$Y$y_2, 
  dim_red_scurve$Y$y_1, 
  make_s_curve(n_points = 500L)[, t]))
setnames(data_plot_scurve, c("x", "y", "t"))

plot_manifold_2d(data_plot_scurve)

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

