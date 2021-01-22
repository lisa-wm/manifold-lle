# ------------------------------------------------------------------------------
# FIND NEAREST NEIGHBORS
# ------------------------------------------------------------------------------

# Purpose: find nearest neighbors to construct neighborhood graph

find_neighbors <- function(data, neighborhood_method, neighborhood_size) {
  
  # COMPUTE DISTANCE RANK MATRIX -----------------------------------------------
  
  dist_matrix <- as.matrix(dist(data), nrow = nrow(data))
  
  if (neighborhood_method == "knn") {
    
    # In case of ties, random selection is performed to keep number of 
    # neighbors constant)
    
    neighbors_ranking <- t(apply(
      dist_matrix, 
      1, 
      data.table::frank, 
      ties.method = "random"))
    
    neighbors_logical <- (
      neighbors_ranking > 1 & neighbors_ranking <= neighborhood_size + 1)
    
    } else {
      
      neighbors_logical <- (dist_matrix > 0 & dist_matrix <= neighborhood_size)  
      
    }
  
  # RETURN ---------------------------------------------------------------------
  
  neighbors_logical

}
