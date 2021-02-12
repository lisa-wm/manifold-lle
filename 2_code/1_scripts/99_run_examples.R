# ------------------------------------------------------------------------------
# DIMENSIONALITY REDUCTION
# ------------------------------------------------------------------------------

# Purpose: perform dimensionality reduction

# In:  data sets to run sslle on
# Out: data sets with embedding coordinates

# DATA -------------------------------------------------------------------------

# GENERATE DATA SETS -----------------------------------------------------------

data_incomplete_tire <- make_incomplete_tire(n_points = 1000L)
data_swiss_roll <- make_swiss_roll(n_points = 1000L)
data_scurve <- make_s_curve(n_points = 1000L)

data_sets <- list(
  incomplete_tire = data_incomplete_tire,
  # , 
  swiss_roll = data_swiss_roll
  # scurve = data_scurve
  )

data_unlabeled <- lapply(data_sets, function(i) {i[, .(x_1, x_2, x_3)]})

# LEM --------------------------------------------------------------------------

# LLE --------------------------------------------------------------------------

# scurve 3000 points, 14 neighbors works top
# more points is def better

swissroll_lle <- perform_lle(
  data_unlabeled$swiss_roll,
  k_max = 0.05 * nrow(data_unlabeled$swiss_roll),
  intrinsic_dim = 2L)

as.data.frame(swissroll_lle$neighborhood_search) %>% 
  ggplot(aes(x = neighborhood_sizes, y = reconstruction_errors)) + 
  geom_line()

swissroll_lle_plot <- data.table(
  swissroll_lle$Y, 
  data_sets$swiss_roll[, .(t, s)])
plot_manifold(swissroll_lle_plot, 2L)

# swiss roll 1500 points, 14 neighbors works fairly well

swiss_roll_lle <- perform_lle(
  data_unlabeled$swiss_roll,
  intrinsic_dim = 2L,
  k_max = 15L)

swiss_roll_lle_plot <- data.table(
  swiss_roll_lle$Y, 
  data_sets$swiss_roll[, .(t, s)])
plot_manifold(swiss_roll_lle_plot, 2L)

# HLLE -------------------------------------------------------------------------

# SSLLE ------------------------------------------------------------------------

# Exact version

incomplete_tire_u <- data_unlabeled$incomplete_tire
incomplete_tire_l <- data_sets$incomplete_tire
k_max <- 15L

prior_points_ind <- find_landmarks(
  incomplete_tire_u,
  n_neighbors = k_max,
  n_landmarks = 3L,
  method = "maxmin")

prior_points <- incomplete_tire_l[prior_points_ind, .(t, s)]

new_order <- c(
  prior_points_ind, 
  setdiff(incomplete_tire_l[, .I], prior_points_ind))

incomplete_tire_sslle <- perform_sslle(
  data = incomplete_tire_l[new_order],
  k_max = k_max,
  prior_points = prior_points)

# as.data.frame(incomplete_tire_sslle$neighborhood_search) %>% 
#   ggplot(aes(x = neighborhood_sizes, y = reconstruction_errors)) + 
#   geom_line()

incomplete_tire_sslle_plot <- data.table(
  incomplete_tire_sslle$Y, 
  incomplete_tire_l[new_order][, .(t, s)])
plot_manifold(incomplete_tire_sslle_plot, 2L)

# Inexact version (simulated with Gaussian noise)

set.seed(1L)
prior_points_noisy <- data_sets$incomplete_tire[prior_points_ind, .(t, s)] +
  rnorm(length(prior_points_ind), sd = 1L)

incomplete_tire_sslle_noisy <- perform_sslle(
  data = incomplete_tire[new_order],
  k_max = k_max,
  prior_points = prior_points_noisy,
  is_exact = FALSE,
  confidence_param = 0.0001)

incomplete_tire_sslle_plot_noisy <- data.table(
  incomplete_tire_sslle_noisy$Y, 
  data_sets$incomplete_tire[new_order][, .(t, s)])
plot_manifold(incomplete_tire_sslle_plot_noisy, 2L)

# Swiss roll

swiss_roll_u <- data_unlabeled$swiss_roll
swiss_roll_l <- data_sets$swiss_roll
k_max <- 15L

prior_points_ind <- find_landmarks(
  swiss_roll_u,
  n_neighbors = k_max,
  n_landmarks = 5L,
  method = "maxmin")

prior_points <- swiss_roll_l[prior_points_ind, .(t, s)]

new_order <- c(
  prior_points_ind, 
  setdiff(swiss_roll_l[, .I], prior_points_ind))

swiss_roll_sslle <- perform_sslle(
  data = swiss_roll_l[new_order],
  k_max = k_max,
  prior_points = prior_points)

swiss_roll_sslle_plot <- data.table(
  swiss_roll_sslle$Y, 
  data_sets$swiss_roll[new_order][, .(t, s)])
plot_manifold(swiss_roll_sslle_plot, 2L)
