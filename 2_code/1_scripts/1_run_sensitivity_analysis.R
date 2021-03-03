# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

data_labeled <- list(
  incomplete_tire = make_incomplete_tire(n_points = 1000L), 
  swiss_roll = make_swiss_roll(n_points = 1000L),
  world_data = make_world_data_3d(
    here("2_code/2_data", "rawdata_world_3d.csv")))

save_rdata_files(data_labeled, folder = "2_code/2_data")

data_unlabeled <- lapply(data_labeled, function(i) {i[, .(x_1, x_2, x_3)]})

true_embeddings <- list(
  incomplete_tire = data_labeled$incomplete_tire[, .(t, s)],
  swiss_roll = data_labeled$swiss_roll[, .(t, s = x_2)],
  world_data = make_world_data_2d(
    here("2_code/2_data", "rawdata_world_2d.csv"))[, .(t = x_1, s = x_2)])

k_max <- 15L
n_landmarks_max <- 12L

# SENSITIVITY ANALYSIS I: CHOICE OF PRIOR POINTS -------------------------------

# Set up combinations of method and number of prior points

search_grid_landmarks <- expand.grid(
  landmark_method = c("poor_coverage", "random_coverage", "maximum_coverage"),
  n_landmarks = seq(2L, n_landmarks_max, by = 2L))

sensitivity_landmarks <- parallel::mclapply(
  
  seq_along(data_unlabeled),
  
  function(i) {
    
    parallel::mclapply(
  
    seq_len(nrow(search_grid_landmarks)),
    
    function(j) {
      
      if (j == 1L | j %% 5L == 0L) cat(sprintf("trying combination %d...\n", j))
      
      # Define required parameters
      
      # data_l <- data_labeled[[i]]
      data_u <- data_unlabeled[[i]]
      true_emb <- true_embeddings[[i]]
      this_method <- as.character(search_grid_landmarks[j, "landmark_method"])
      this_number <- search_grid_landmarks[j, "n_landmarks"]
      
      # Find prior points according to current method and number
      
      landmarks_ind <- switch(
        this_method,
        poor_coverage = order(true_emb$t)[seq_len(this_number)],
        random_coverage = find_landmarks(
          data = data_u,
          n_landmarks = this_number,
          method = "random"),
        maximum_coverage = find_landmarks(
          data = data_u,
          n_neighbors = k_max,
          n_landmarks = this_number,
          method = "maxmin"))
      
      landmarks <- true_emb[landmarks_ind, .(t, s)]
      
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
        residual_variance = min(embedding$residual_variances),
        auc_lnk_rnx = max(embedding$auc_lnk_rnx),
        embedding_result = embedding,
        landmarks = landmarks, 
        true_embedding = true_emb[new_order])
      
    },
    
    mc.cores = parallel::detectCores())
    
  }, 
  
  mc.cores = parallel::detectCores()

)

sensitivity_landmarks_dt <- lapply(
  seq_along(sensitivity_landmarks), 
  function(i) {
    dt <- data.table::as.data.table(do.call(rbind, sensitivity_landmarks[[i]]))
    dt[, names(dt)[1:4] := lapply(.SD, unlist), .SDcols = names(dt)[1:4]]
    dt[, landmark_method := as.factor(landmark_method)]
    dt})

names(sensitivity_landmarks_dt) <- names(data_labeled)

save_rdata_files(sensitivity_landmarks_dt, folder = "2_code/2_data")

# SENSITIVITY ANALYSIS II: NOISE LEVEL & NUMBER OF PRIOR POINTS ----------------

search_grid_noise <- expand.grid(
  noise_level = c(0.1, 0.5, 1L, 3L),
  n_landmarks = seq(2L, n_landmarks_max, by = 2L))

sensitivity_noise <- parallel::mclapply(
  
  seq_along(data_unlabeled),
  
  function(i) {
    
    parallel::mclapply(
      
      seq_len(nrow(search_grid_noise)),
      
      function(j) {
        
        if (j == 1L | j %% 10 == 0) {
          cat(sprintf("trying combination %d...\n", j))}
        
        # Define required parameters
        
        data_l <- data_labeled[[i]]
        data_u <- data_unlabeled[[i]]
        this_noise <- search_grid_noise[j, "noise_level"]
        this_number <- search_grid_noise[j, "n_landmarks"]
        
        # Find prior points and apply perturbation
        
        landmarks_ind <- find_landmarks(
          data = data_u,
          n_neighbors = k_max,
          n_landmarks = this_number,
          method = "maxmin")
        
        landmarks <- data_l[landmarks_ind, .(t, s)]
        
        sd_t = sd(data_l$t)
        sd_s = sd(data_l$s)
        
        landmarks_corrupted <- landmarks + c(
          rnorm(length(landmarks_ind), sd = this_noise * sd_t),
          rnorm(length(landmarks_ind), sd = this_noise * sd_s))
        
        # Move prior points up in the data as sslle function assumes the the 
        # first observations to be prior points
        
        new_order <- c(landmarks_ind, setdiff(data_u[, .I], landmarks_ind))
        
        # Compute embedding
        
        embedding <- perform_sslle(
          data = data_l[new_order],
          k_max = k_max,
          prior_points = landmarks_corrupted,
          is_exact = FALSE,
          confidence_param = 0.01,
          verbose = FALSE)
        
        # Return
        
        list(
          noise_level = this_noise,
          n_landmarks = this_number,
          residual_variance = min(embedding$residual_variances),
          auc_lnk_rnx = max(embedding$auc_lnk_rnx),
          embedding_result = embedding,
          landmarks = landmarks)
        
      },
      
      mc.cores = parallel::detectCores())
    
  }, 
  
  mc.cores = parallel::detectCores()
  
)

sensitivity_noise_dt <- lapply(
  seq_along(sensitivity_noise), 
  function(i) {
    dt <- data.table::as.data.table(do.call(rbind, sensitivity_noise[[i]]))
    dt[, names(dt)[1:4] := lapply(.SD, unlist), .SDcols = names(dt)[1:4]]
    dt})

names(sensitivity_noise_dt) <- names(data_labeled)

save_rdata_files(sensitivity_noise_dt, folder = "2_code/2_data")

# SENSITIVITY ANALYSIS II: NOISE LEVEL & CONFIDENCE PARAM ----------------------

# search_space_noise <- list(
#   noise_level = c(lower = 0L, upper = 1L),
#   confidence_param = c(lower = 10e-10, upper = 10e-1))

# search_grid_noise <- expand.grid(
#   noise_level = c(0.1, 0.5, 1L, 5L, 10L),
#   confidence_param = sapply(seq(-4L, 2L, length.out = 7), function(i) 10^(i)))
# 
# sensitivity_noise <- parallel::mclapply(
#   
#   seq_along(data_unlabeled),
#   
#   function(i) {
#     
#     parallel::mclapply(
#       
#       seq_len(nrow(search_grid_noise)),
#       
#       function(j) {
#         
#         if (j == 1L | j %% 10 == 0) {
#           cat(sprintf("trying combination %d...\n", j))}
#         
#         # Define required parameters
#         
#         data_l <- data_labeled[[i]]
#         data_u <- data_unlabeled[[i]]
#         this_noise <- search_grid_noise[j, "noise_level"]
#         this_confidence <- search_grid_noise[j, "confidence_param"]
#         
#         # Find prior points and apply perturbation
#         
#         landmarks_ind <- find_landmarks(
#           data = data_u,
#           n_neighbors = k_max,
#           n_landmarks = 5L,
#           method = "maxmin")
#         
#         landmarks <- data_l[landmarks_ind, .(t, s)] +
#           rnorm(length(landmarks_ind), sd = this_noise)
#         
#         # Move prior points up in the data as sslle function assumes the the 
#         # first observations to be prior points
#         
#         new_order <- c(landmarks_ind, setdiff(data_u[, .I], landmarks_ind))
#         
#         # Compute embedding
# 
#         embedding <- perform_sslle(
#           data = data_l[new_order],
#           k_max = k_max,
#           prior_points = landmarks,
#           is_exact = FALSE,
#           confidence_param = this_confidence,
#           verbose = FALSE)
#         
#         # Return
#         
#         list(
#           noise_level = this_noise,
#           confidence_param = this_confidence,
#           residual_variance = min(embedding$residual_variances),
#           auc_lnk_rnx = max(embedding$auc_lnk_rnx),
#           embedding_result = embedding)
#         
#       },
#       
#       mc.cores = parallel::detectCores())
#     
#   }, 
#   
#   mc.cores = parallel::detectCores()
#   
# )
# 
# sensitivity_noise_dt <- lapply(
#   seq_along(sensitivity_noise), 
#   function(i) {
#     dt <- data.table::as.data.table(do.call(rbind, sensitivity_noise[[i]]))
#     dt[, names(dt)[1:4] := lapply(.SD, unlist), .SDcols = names(dt)[1:4]]
#     dt})
# 
# names(sensitivity_noise_dt) <- names(data_labeled)
# 
# save_rdata_files(sensitivity_noise_dt, "2_code")

