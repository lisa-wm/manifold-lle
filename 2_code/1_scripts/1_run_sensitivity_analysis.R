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

sensitivity_landmarks <- parallel::mclapply(
  
  seq_along(data_unlabeled),
  
  function(i) {
    
    parallel::mclapply(
  
    seq_len(nrow(search_grid_landmarks)),
    
    function(j) {
      
      if (j == 1L | j %% 10 == 0) cat(sprintf("trying combination %d...\n", j))
      
      # Define required parameters
      
      data_l <- data_labeled[[i]]
      data_u <- data_unlabeled[[i]]
      this_method <- as.character(search_grid_landmarks[j, "landmark_method"])
      this_number <- search_grid_landmarks[j, "n_landmarks"]
      k_max <- 15L
      
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
        auc_lnk_rnx = max(embedding$auc_lnk_rnx),
        embedding_result = embedding)
      
    },
    
    mc.cores = parallel::detectCores())
    
  }, 
  
  mc.cores = parallel::detectCores()

)

sensitivity_landmarks_dt <- lapply(
  seq_along(sensitivity_landmarks), 
  function(i) {
    dt <- data.table::as.data.table(do.call(rbind, sensitivity_landmarks[[i]]))
    dt[, names(dt)[1:3] := lapply(.SD, unlist), .SDcols = names(dt)[1:3]]
    dt[, landmark_method := as.factor(landmark_method)]
    dt})

names(sensitivity_landmarks_dt) <- names(data_labeled)

save_rdata_files(sensitivity_landmarks_dt, "2_code")

sensitivity_landmarks_plots <- lapply(
  seq_along(sensitivity_landmarks_dt),
  function(i) {
    sensitivity_landmarks_dt[[i]][
      , .(landmark_method, n_landmarks, auc_lnk_rnx)] %>% 
      ggplot2::ggplot(aes(
        x = forcats::fct_relevel(
          landmark_method, 
          "poor_coverage",
          "random_coverage",
          "optimal_coverage"), 
        y = n_landmarks, 
        col = auc_lnk_rnx)) +
      geom_point(size = 5L) +
      scale_color_gradient(low = "red", high = "green") +
      ylab("number of landmarks") +
      xlab("") +
      ggtitle(sprintf("Method: %s", names(sensitivity_landmarks[i])))})

for (i in seq_along(sensitivity_landmarks_dt)) {
  
  sensitivity_landmarks_dt[[i]][
    , .(landmark_method, n_landmarks, auc_lnk_rnx)] %>% 
    ggplot2::ggplot(aes(
      x = forcats::fct_relevel(
        landmark_method, 
        "poor_coverage",
        "random_coverage",
        "optimal_coverage"), 
      y = n_landmarks, 
      col = auc_lnk_rnx)) +
    geom_point(size = 5L) +
    scale_color_gradient(low = "red", high = "green")
  
}

# SENSITIVITY ANALYSIS II: NOISE LEVEL & CONFIDENCE ----------------------------

search_grid_noise <- expand.grid(
  noise_level = seq(0L, 3L, length.out = 10L),
  confidence_param = sapply(seq_len(10L), function(i) 10^(-i)))

search_space_noise <- list(
  noise_level = c(lower = 0L, upper = 3L),
  confidence_param = c(lower = 10e-1, upper = 10e-10))

set.seed(123L)

sensitivity_noise <- lapply(
  
  seq_len(50L),
  
  function(i) {
    
    if (i == 1L | i %% 10 == 0) cat(sprintf("trying combination %d...\n", i))
    
    # Define required parameters
    
    data_l <- data_labeled$incomplete_tire
    data_u <- data_unlabeled$incomplete_tire
    this_noise <- runif(
      1L, 
      min = search_space_noise$noise_level["lower"],
      max = search_space_noise$noise_level["upper"])
    this_confidence <- runif(
      1L, 
      min = search_space_noise$confidence_param["lower"],
      max = search_space_noise$confidence_param["upper"])
    k_max <- 20L
    
    # Find prior points according to current method and number
    
    landmarks_ind <- find_landmarks(
        data = data_u,
        n_neighbors = k_max,
        n_landmarks = 20L,
        method = "maxmin")
    
    landmarks <- data_l[landmarks_ind, .(t, s)] +
      rnorm(length(landmarks_ind), sd = this_noise)
    
    # Move prior points up in the data as sslle function assumes the the first
    # observations to be prior points
    
    new_order <- c(landmarks_ind, setdiff(data_u[, .I], landmarks_ind))
    
    # Compute embedding
    
    embedding <- perform_sslle(
      data = data_u[new_order],
      k_max = k_max,
      prior_points = landmarks,
      is_exact = FALSE,
      confidence_param = this_confidence,
      verbose = FALSE)
    
    # Return
    
    list(
      noise_level = this_noise,
      confidence_param = this_confidence,
      auc_lnk_rnx = max(embedding$auc_lnk_rnx))
    
  }
  
)