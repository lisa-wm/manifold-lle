# ------------------------------------------------------------------------------
# FINDING LANDMARKS
# ------------------------------------------------------------------------------

# Purpose: find landmarks to use as prior points in sslle

find_landmarks <- function(data,
                           n_landmarks, 
                           method = c("random", "maxmin"),
                           n_neighbors = NULL,
                           seed = 1L) {
  
  # CHECK INPUTS ---------------------------------------------------------------
  
  dt <- data.table::as.data.table(data)
  method <- match.arg(method, c("random", "maxmin"))
  checkmate::assert_count(n_landmarks)
  checkmate::assert_count(n_neighbors, null.ok = TRUE)
  
  if (method == "maxmin" & is.null(n_neighbors)) {
    stop("number of neighbors must be specified for maxmin method")}

  # FIND LANDMARKS -------------------------------------------------------------
  
  indices <- dt[, .I]
  
  if (method == "random") {
    
    set.seed(seed)
    landmarks <- sample(indices, n_landmarks, replace = FALSE)
    
  } else {
    
    cat("computing geodesics...\n")
    
    distances_geodesic <- dimRed::embed(
      dt,
      "Isomap",
      .mute = "message",
      knn = n_neighbors,
      get_geod = TRUE)@other.data$geod
    
    cat("finding landmarks...\n")
    
    distance_matrix_geodesic <- as.matrix(distances_geodesic, nrow = nrow(dt))

    set.seed(seed)
    landmarks <- sample(indices, 1L)
    
    while (length(landmarks) < n_landmarks) {
      
      # Restrict search to interesting area to make as fast as possible
      
      search_area <- as.matrix(
        distance_matrix_geodesic[-landmarks, landmarks],
        ncol = length(landmarks))
      min_distance <- apply(search_area, 1L, min)
      next_landmark <- as.integer(names(which.max(min_distance)))
      
      landmarks <- c(landmarks, next_landmark)
      
    }
    
  }
  
  sort(unname(landmarks))
  
}
