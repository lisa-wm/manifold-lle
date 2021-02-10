# ------------------------------------------------------------------------------
# FIND NEAREST NEIGHBORS
# ------------------------------------------------------------------------------

# Purpose: find nearest neighbors to construct neighborhood graph

find_neighbors <- function(data, neighborhood_size) {
  
  # COMPUTE DISTANCE RANK MATRIX -----------------------------------------------
  
  dist_matrix <- as.matrix(dist(data), nrow = nrow(data))
  
  # In case of ties, random selection is performed to keep number of 
  # neighbors constant)
  
  neighbors_ranking <- t(apply(
    dist_matrix, 
    1L, 
    data.table::frank, 
    ties.method = "random"))
  
  neighbors_logical <- (
    neighbors_ranking > 1 & neighbors_ranking <= neighborhood_size + 1)
  
  # RETURN ---------------------------------------------------------------------
  
  neighbors_logical

}
