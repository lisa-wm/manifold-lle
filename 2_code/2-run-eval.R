# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

data_labeled <- list(
  incomplete_tire = make_incomplete_tire(n_points = 1000L), 
  swiss_roll = make_swiss_roll(n_points = 1000L),
  scurve = make_s_curve(n_points = 1000L))

data_unlabeled <- lapply(data_labeled, function(i) {i[, .(x, y, z)]})

# SENSITIVITY ANALYSIS I: CHOICE OF PRIOR POINTS -------------------------------

search_grid_prior_points <- expand.grid(
  landmark_method = c("poor_coverage", "random_coverage", "optimal_coverage"),
  n_prior_points = seq_len(20L))



# SENSITIVITY ANALYSIS II: NOISE LEVEL & CONFIDENCE ----------------------------

search_grid_noise <- expand.grid(
  noise_level = seq(0L, 3L, length.out = 10L),
  confidence_param = sapply(seq_len(10L), function(i) 10^(-i)))
