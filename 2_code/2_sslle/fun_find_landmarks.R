# ------------------------------------------------------------------------------
# FINDING LANDMARKS
# ------------------------------------------------------------------------------

# Purpose: find landmarks to use as prior points in sslle

find_landmarks <- function(data,
                           n_neighbors = NULL,
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
    
    cat("computing geodesics...\n")
    
    distances_geodesic <- invisible(dimRed::embed(
      data,
      "Isomap",
      .mute = "message",
      knn = k_max,
      get_geod = TRUE)@other.data$geod)
    
    cat("finding landmarks...\n")
    
    distance_matrix_geodesic <- as.matrix(distances_geodesic, nrow = nrow(data))

    set.seed(seed)
    landmarks <- sample(indices, 1L)
    
    while (length(landmarks) < n_landmarks) {
      
      # Restrict search to interesting area to make as fast as possible
      
      search_area <- as.matrix(
        distance_matrix_geodesic[-landmarks, landmarks],
        ncol = length(landmarks))
      min_distance <- apply(search_area, 1, min)
      next_landmark <- as.integer(names(which.max(min_distance)))
      
      landmarks <- c(landmarks, next_landmark)
      
    }
    
  }
  
  sort(unname(landmarks))
  
}
