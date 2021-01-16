# ------------------------------------------------------------------------------
# LLE IMPLEMENTATION: NEIGHBORHOOD COMPUTATION
# ------------------------------------------------------------------------------

# TODO Handle case where too few or too many neighbors are selected

find_neighbors <- function(data, is_knn, neighborhood_size) {
  
  dist_matrix <- as.matrix(dist(data), nrow = nrow(data))
  
  if (is_knn) {
    
    neighbors_ranking <- t(apply(
      dist_matrix, 
      1, 
      data.table::frank, 
      ties.method = "random"))
    
    neighbors_logical <- (
      neighbors_ranking > 1 & neighbors_ranking <= neighborhood_size + 1)
    
    } else {
    
      neighbors_logical <- (
        dist_matrix > 0 & dist_matrix <= neighborhood_size)  
      
    }
  
  neighbors_logical

}
