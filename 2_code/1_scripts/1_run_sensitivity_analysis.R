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

# Set up combinations of method and number of prior points

search_grid_landmarks <- expand.grid(
  landmark_method = c("poor_coverage", "random_coverage", "optimal_coverage"),
  n_landmarks = seq_len(20L))

sensitivity_landmarks <- lapply(
  
  seq_len(nrow(search_grid_landmarks)),
  
  function(i) {
    
    if (i == 1L | i %% 10) cat(sprintf("trying combination %d...\n", i))
    
    # Define required parameters
    
    data_l <- data_labeled$incomplete_tire
    data_u <- data_unlabeled$incomplete_tire
    this_method <- as.character(search_grid_landmarks[i, "landmark_method"])
    this_number <- search_grid_landmarks[i, "n_landmarks"]
    k_max <- 20L
    
    # Find prior points according to current method and number
    
    landmarks_ind <- switch(
      this_method,
      poor_coverage = order(data_l$t)[seq_len(this_number)],
      random_coverage = find_landmarks(
        data = data_u,
        n_landmarks = this_number,
        method = "random"),
      optimal_coverage = find_landmarks(
        data = data_u,
        n_neighbors = k_max,
        n_landmarks = this_number,
        method = "maxmin"))
    
    landmarks <- data_l[landmarks_ind, .(t, s)]
    
    # Move prior points up in the data as sslle function assumes the the first
    # observations to be prior points
    
    new_order <- c(landmarks_ind, setdiff(data_u[, .I], landmarks_ind))
    
    # Compute embedding
    
    embedding <- perform_sslle(
      data = data_u[new_order],
      k_max = k_max,
      prior_points = landmarks,
      verbose = FALSE)
    
    # Return
    
    list(
      landmark_method = this_method,
      n_landmarks = this_number,
      auc_lnk_rnx = max(embedding$auc_lnk_rnx))
    
  }
)

sensitivity_landmarks_dt <- data.table::as.data.table(
  do.call(rbind, sensitivity_landmarks))

sensitivity_landmarks_dt[
  , names(sensitivity_landmarks_dt) := lapply(sensitivity_landmarks_dt, unlist)]

sensitivity_landmarks_dt[, landmark_method := as.factor(landmark_method)]

sensitivity_landmarks_dt %>% 
  ggplot(aes(x = landmark_method, y = n_landmarks, col = auc_lnk_rnx)) +
  geom_point(size = 5L) +
  scale_color_gradient(low = "red", high = "green")

# SENSITIVITY ANALYSIS II: NOISE LEVEL & CONFIDENCE ----------------------------

search_grid_noise <- expand.grid(
  noise_level = seq(0L, 3L, length.out = 10L),
  confidence_param = sapply(seq_len(10L), function(i) 10^(-i)))
