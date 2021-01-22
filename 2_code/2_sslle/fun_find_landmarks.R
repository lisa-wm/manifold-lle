# ------------------------------------------------------------------------------
# FINDING LANDMARKS
# ------------------------------------------------------------------------------

# Purpose: find landmarks to use as prior points in sslle

find_landmarks <- function(data, 
                           n_landmarks, 
                           method = c("random", "maxmin"),
                           seed = 123L) {
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  check_data(data)
  data <- as.data.table(data)

  if (!checkmate::test_count(n_landmarks)) {
    stop("number of landmarks must be a positive integer")
  }
  
  method <- match.arg(method, c("random", "maxmin"))

  # FIND LANDMARKS -------------------------------------------------------------
  
  indices <- data[, .I]
  
  if (method == "random") {
    
    set.seed(seed)
    landmarks <- sample(indices, n_landmarks, replace = FALSE)
    
  } else {
    
    # TODO make sure this does the right thing!!
    
    distances_geodesic <- invisible(dimRed::embed(
      data,
      "Isomap",
      .mute = "message",
      knn = nrow(data) - 1L,
      get_geod = TRUE)@other.data$geod)
    
    distance_matrix_geodesic <- as.matrix(distances_geodesic, nrow = nrow(data))
    
    distance_ranking <- t(apply(
      -distance_matrix_geodesic, 
      1, 
      data.table::frank, 
      ties.method = "random"))
    
    set.seed(seed)
    id <- sample(indices, 1L)
    landmarks <- id

    while (length(landmarks) < n_landmarks) {
      
      dist_vecs <- as.matrix(
        distance_matrix_geodesic[, landmarks],
        ncol = length(landmarks))
      min_distance <- apply(dist_vecs, 1, min)
      next_landmark <- which.max(min_distance)
      
      landmarks <- c(landmarks, next_landmark)
      
    }

  }
  
  unname(landmarks)
  
}
