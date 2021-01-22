# ------------------------------------------------------------------------------
# DIMENSIONALITY REDUCTION
# ------------------------------------------------------------------------------

# Purpose: perform dimensionality reduction

# In:  data sets to run sslle on
# Out: data sets with embedding coordinates

# LEM --------------------------------------------------------------------------

# LLE --------------------------------------------------------------------------



# HLLE -------------------------------------------------------------------------

# SSLLE ------------------------------------------------------------------------


# FIND NEIGHBORS ---------------------------------------------------------------

optimal_

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