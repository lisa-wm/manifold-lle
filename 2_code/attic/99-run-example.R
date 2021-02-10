# ------------------------------------------------------------------------------
# EXAMPLE TO TEST LLE FUNCTIONS
# ------------------------------------------------------------------------------

# SWISS ROLL EXAMPLE -----------------------------------------------------------

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

plot_manifold(
  as.data.table(cbind(result_them$Y[, 2], result_them$Y[, 1], data_sr[, t])), 
  dim = 2L)
plot_manifold(
  as.data.table(cbind(result_me$Y$y_2, result_me$Y$y_1, data_sr[, t])), 
  dim = 2L)

# S-CURVE EXAMPLE --------------------------------------------------------------

scurve_2d <- compute_lle(
  make_s_curve(n_points = 1000L),
  intrinsic_dim = 2L,
  is_knn = TRUE,
  neighborhood_size = 15L
)

plot_manifold(
  as.data.table(cbind(
    scurve_2d$Y$y_2, 
    scurve_2d$Y$y_1, 
    make_s_curve(n_points = 1000L)[, t])),
  dim = 2L
)
