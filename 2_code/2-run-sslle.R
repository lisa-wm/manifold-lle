# ------------------------------------------------------------------------------
# DIMENSIONALITY REDUCTION
# ------------------------------------------------------------------------------

# Purpose: perform dimensionality reduction

# In:  data sets to run sslle on
# Out: data sets with embedding coordinates

# DATA -------------------------------------------------------------------------

# TODO work on automatic selection of k
# TODO what about epsilon neighborhoods
# TODO set up benchmark study with varying combos of samples and neigbhors
# (and landmarks!)
# TODO find out about rev and abs in returning embedding coordinates
# TODO incorporate landmark lle / nystrom approx
# TODO incorporate inexact prior information

load_rdata_files(data_sets, folder = "2_code/1_data")
data_unlabeled <- lapply(data_sets, function(i) {i[, .(x, y, z)]})

# LEM --------------------------------------------------------------------------

# LLE --------------------------------------------------------------------------

# scurve 3000 points, 18 neighbors works fairly well
# more points is def better

scurve_lle <- perform_lle(
  data_unlabeled$scurve,
  intrinsic_dim = 2L,
  neighborhood_method = "knn",
  neighborhood_size = 18L
)

scurve_lle_plot <- data.table(scurve_lle$Y, data_sets$scurve[, .(t, s)])
plot_manifold(scurve_lle_plot, 2L)

# swiss roll 1500 points, 14 neighbors works fairly well

swiss_roll_lle <- perform_lle(
  data_unlabeled$swiss_roll,
  intrinsic_dim = 2L,
  neighborhood_method = "knn",
  neighborhood_size = 14L
)

swiss_roll_lle_plot <- data.table(
  swiss_roll_lle$Y, 
  data_sets$swiss_roll[, .(t, s)])
plot_manifold(swiss_roll_lle_plot, 2L)

# HLLE -------------------------------------------------------------------------

# SSLLE ------------------------------------------------------------------------

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
  neighborhood_size = 13L
)

incomplete_tire_lle_plot <- data.table(
  incomplete_tire_sslle$Y, 
  data_sets$incomplete_tire[new_order][, .(t, s)])
plot_manifold(incomplete_tire_lle_plot, 2L)

# FIND NEIGHBORS ---------------------------------------------------------------

# find optimal number of neighbors (what about epsilon???)

optimal_k <- find_k(foo)

# find neighbors

neighbors <- find_neighbors(method = c("knn", "epsilon"))

# FIND LANDMARK POINTS ---------------------------------------------------------



# find landmark points
# to be used as for landmark lle or as prior points in sslle

landmarks <- find_landmarks(method = c("random", "maxmin"))

# compute reconstruction weights

weights <- compute_weights(regu = c("none", "foo", "poo"))

# compute embedding matrix
# from entire data or landmarks
# w/ specific forms of regu (the one currently in use is also in the ghojogh
# tutorial)
# or from unlabeled points only

embedding_mat <- compute_embedding_matrix(
  landmark = c(TRUE, FALSE), 
  semisupervised = c(TRUE, FALSE))

# find intrinsic dim
# how???

# compute embedding coordinates

embedding_coord <- find_embedding_coordinates()

# evaluate

result <- eval_dim_red(method = c("foo", "poo"))

# plot

plot(manifold)