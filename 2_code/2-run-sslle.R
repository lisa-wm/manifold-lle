# ------------------------------------------------------------------------------
# DIMENSIONALITY REDUCTION
# ------------------------------------------------------------------------------

# Purpose: perform dimensionality reduction

# In:  data sets to run sslle on
# Out: data sets with embedding coordinates

# DATA -------------------------------------------------------------------------

# TODO work on automatic selection of k
# TODO what about epsilon neighborhoods
# TODO find out about rev and abs in returning embedding coordinates
# TODO incorporate landmark lle / nystrom approx

load_rdata_files(data_sets, folder = "2_code/1_data")
data_unlabeled <- lapply(data_sets, function(i) {i[, .(x, y, z)]})

# LEM --------------------------------------------------------------------------

# LLE --------------------------------------------------------------------------

# scurve 3000 points, 14 neighbors works top
# more points is def better

scurve_lle <- perform_lle(
  data_unlabeled$scurve,
  intrinsic_dim = 2L,
  neighborhood_method = "knn",
  choices_k = 14L:18L
)

scurve_lle_plot <- data.table(scurve_lle$Y, data_sets$scurve[, .(t, s)])
plot_manifold(scurve_lle_plot, 2L)

# swiss roll 1500 points, 14 neighbors works fairly well

swiss_roll_lle <- perform_lle(
  data_unlabeled$swiss_roll,
  intrinsic_dim = 2L,
  neighborhood_method = "knn",
  choices_k = 12L:16L
)

swiss_roll_lle_plot <- data.table(
  swiss_roll_lle$Y, 
  data_sets$swiss_roll[, .(t, s)])
plot_manifold(swiss_roll_lle_plot, 2L)

# HLLE -------------------------------------------------------------------------

# SSLLE ------------------------------------------------------------------------

# Exact version

incomplete_tire <- data_unlabeled$incomplete_tire

prior_points_ind <- find_landmarks(
  incomplete_tire,
  n_landmarks = 10L,
  method = "maxmin"
)

prior_points <- data_sets$incomplete_tire[prior_points_ind, .(t, s)]

new_order <- c(
  prior_points_ind, 
  setdiff(incomplete_tire[, .I], prior_points_ind))

incomplete_tire_sslle <- perform_sslle(
  data = incomplete_tire[new_order],
  prior_points = prior_points,
  neighborhood_method = "knn",
  choices_k = 8L:10L,
  regularization_param = 0.001
)

incomplete_tire_sslle_plot <- data.table(
  incomplete_tire_sslle$Y, 
  data_sets$incomplete_tire[new_order][, .(t, s)])
plot_manifold(incomplete_tire_sslle_plot, 2L)

# Inexact version (simulated with Gaussian noise)

set.seed(1L)
prior_points_noisy <- data_sets$incomplete_tire[prior_points_ind, .(t, s)] +
  rnorm(length(prior_points_ind), sd = .5)

incomplete_tire_sslle_noisy <- perform_sslle(
  data = incomplete_tire[new_order],
  prior_points = prior_points_noisy,
  neighborhood_method = "knn",
  choices_k = 8L:10L,
  regularization_param = 0.001,
  is_exact = FALSE,
  confidence_param = 0.001
)

incomplete_tire_sslle_plot_noisy <- data.table(
  incomplete_tire_sslle_noisy$Y, 
  data_sets$incomplete_tire[new_order][, .(t, s)])
plot_manifold(incomplete_tire_sslle_plot_noisy, 2L)