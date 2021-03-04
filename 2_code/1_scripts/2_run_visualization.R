# ------------------------------------------------------------------------------
# VISUALIZATION OF RESULTS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

load_rdata_files(sensitivity_landmarks_dt, folder = "2_code/2_data")
load_rdata_files(sensitivity_noise_dt, folder = "2_code/2_data")
load_rdata_files(data_labeled, folder = "2_code/2_data")

# VISUALIZATION: AUC_LNK_RNX ---------------------------------------------------

sensitivity_plots_auc <- lapply(
  
  seq_along(data_labeled),
  
  function(i) {
    
    # Make grid plot theme
    
    mk_theme <- function(base_plot, 
                         dt_name,
                         legend_title,
                         x_lab,
                         low = "red",
                         high = "green") {
      
      base_plot +
        ggplot2::scale_color_gradient(
          low = low,  
          high = high,
          name = legend_title,
          limits = c(0L, 1L)) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 20)) +
        ggplot2::scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
        ggplot2::xlab(as.character(x_lab)) +
        ggplot2::ylab("number of landmarks") +
        ggplot2::ggtitle(sprintf("Data: %s", dt_name))
      
    }
    
    auc_plot_landmarks <- mk_theme(
      base_plot = sensitivity_landmarks_dt[[i]][
        , .(landmark_method, n_landmarks, auc_lnk_rnx)
        ][, auc_scaled := scale_zero_one(auc_lnk_rnx)] %>%
        ggplot2::ggplot(aes(
          x = forcats::fct_relevel(
            landmark_method,
            "poor_coverage",
            "random_coverage",
            "maximum_coverage"),
          y = n_landmarks,
          col = auc_scaled)) + 
        geom_point(size = 10L) + 
        scale_x_discrete(labels = c("poor", "random", "maximum")),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = expression(AUC(R[NX]) ~ " (scaled)"),
      x_lab = "coverage")
    
    auc_plot_noise <- plot_quant_res(
      base_plot = sensitivity_noise_dt[[i]][
        , .(noise_level, n_landmarks, auc_lnk_rnx)
        ][, auc_scaled := scale_zero_one(auc_lnk_rnx)] %>%
        ggplot2::ggplot(aes(
          x = noise_level,
          y = n_landmarks,
          col = auc_scaled)) +
        geom_point(size = 10L) +
        scale_x_continuous(breaks = c(0.1, 0.5, 1L, 3L)),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = expression(AUC(R[NX]) ~ " (scaled)"),
      x_lab = "noise level")
    
    list(
      auc_plot_landmarks = auc_plot_landmarks, 
      auc_plot_noise = auc_plot_noise)
    
  }
  
)

names(sensitivity_plots_auc) <- names(sensitivity_landmarks_dt)

save_rdata_files(sensitivity_plots_auc, folder = "2_code/2_data")

# VISUALIZATION: LOW-DIMENSIONAL EMBEDDING -------------------------------------

sensitivity_plots_emb <- lapply(
  
  seq_along(data_labeled),
  
  function(i) {
    
    mk_theme <- function(data, intrinsic_coords, dt_name, annotation_text) {
      
      plot_manifold(
        data, 
        intrinsic_coords,
        point_size = 3L) %>% 
        layout(annotations = list(
          text = annotation_text,
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = -0.25,
          showarrow = FALSE))
      
    }
    
    emb_plots_landmarks <- lapply(
      
      seq_len(nrow(sensitivity_landmarks_dt[[i]])),
      
      function(j) {
        
        dt_lm <- data.table::copy(sensitivity_landmarks_dt[[i]])
        data.table::setorder(dt_lm, -n_landmarks)
        dt_lm_name <- names(sensitivity_landmarks_dt)[i]
        
        mk_theme(
          data = dt_lm[j, ]$embedding_result[[1]]$Y,
          intrinsic_coords = dt_lm[j, ]$true_embedding[[1]][, .(t)],
          dt_name = names(sensitivity_landmarks_dt)[i],
          annotation_text = sprintf(
            "%s coverage, %d landmarks",
            unlist(stringr::str_split(dt_lm[j, ]$landmark_method, "_"))[1L],
            dt_lm[j, ]$n_landmarks))})
    
    emb_plots_noise <- lapply(
        
        seq_len(nrow(sensitivity_noise_dt[[i]])),
        
        function(j) {
          
          dt_n <- data.table::copy(sensitivity_noise_dt[[i]])
          data.table::setorder(dt_n, -n_landmarks)
          dt_n_name <- names(sensitivity_noise_dt)[i]
          
          mk_theme(
            data = dt_n[j, ]$embedding_result[[1]]$Y,
            intrinsic_coords = dt_n[j, ]$true_embedding[[1]][, .(t)],
            dt_name = names(sensitivity_noise_dt)[i],
            annotation_text = sprintf(
              "noise %.1f, %d landmarks",
              dt[j, ]$noise_level,
              dt[j, ]$n_landmarks))})
    
    list(
      emb_plots_landmarks = plotly::subplot(
        emb_plots_landmarks,
        nrows = 6L) %>% 
        hide_guides(),
      emb_plots_noise = plotly::subplot(
        emb_plots_noise,
        nrows = 6L) %>% 
        hide_guides())
    
  }
  
)

names(sensitivity_plots_emb) <- names(data_labeled)

save_rdata_files(sensitivity_plots_emb, folder = "2_code/2_data")

# VISUALIZATION: KEY VARIATION -------------------------------------------------

true_embeddings <- list(
  incomplete_tire = data_labeled$incomplete_tire[, .(t, s)],
  swiss_roll = data_labeled$swiss_roll[, .(t, s = x_2)])

sensitivity_plots_key <- lapply(
  
  seq_along(data_labeled),
  
  function(i) {
    
    base_plot <- plot_manifold(
      data = true_embeddings[[i]],
      intrinsic_coords = true_embeddings[[i]][, .(t)],
      point_size = 5L) 
    
    landmarks <- lapply(
      list(
        landmarks_poor = "poor_coverage", 
        landmarks_random = "random_coverage", 
        landmarks_maximum = "maximum_coverage"),
      function(j) {
        data.table::as.data.table(sensitivity_landmarks_dt[[i]][
          landmark_method == j & n_landmarks == 12L]$landmarks)})
    
    plots_coverage <- lapply(
      seq_along(landmarks), 
      function(j) {
        symbols <- c("circle", "x", "star")
        base_plot %>%
          plotly::add_trace(
            x = ~ landmarks[[j]]$y_1,
            y = ~ landmarks[[j]]$y_2,
            color = ~ 1L,
            type = "scatter",
            mode = "markers",
            marker = list(
              color = "black", 
              size = 10L, 
              symbol = symbols[j]))})
   
    plot_coverage <- plotly::subplot(
      plots_coverage,
      nrows = 1L) %>% 
      hide_guides()
    
    # --------------------------------------------------------------------------
    
    landmarks_n <- data.table::as.data.table(sensitivity_noise_dt[[i]][
      noise_level == 0.1 & n_landmarks == 12L]$landmarks)
    
    sd_s = sd(true_embeddings[[i]]$s)
    sd_t = sd(true_embeddings[[i]]$t)
    
    base_plot_n <- base_plot %>% 
      add_trace(
        x = ~ landmarks_n$t,
        y = ~ landmarks_n$s,
        color = ~ 1L,
        type = "scatter",
        mode = "markers",
        marker = list(color = "black", size = 10L)) %>% 
      hide_guides()

    plots_noise <- lapply(
      
      seq_along(unique(sensitivity_noise_dt[[i]]$noise_level)), 
      
      function(j) {
        
        noise_level <- unique(sensitivity_noise_dt[[i]]$noise_level)[j]
        
        base_plot_n %>% 
          layout(shapes = list(
            type = "circle",
            xref = "x",
            yref = "y",
            x0 = landmarks_n[12L]$t - noise_level * sd_t,
            x1 = landmarks_n[12L]$t + noise_level * sd_t,
            y0 = landmarks_n[12L]$s - noise_level * sd_s,
            y1 = landmarks_n[12L]$s + noise_level * sd_s,
            fillcolor = "black",
            opacity = 0.4,
            line = list(color = "black")
          ))
        
      })
    
    plot_noise <- plotly::subplot(
      plots_noise,
      nrows = 1L) %>% 
      hide_guides()
 
    # --------------------------------------------------------------------------
    
    list(plot_coverage = plot_coverage, plot_noise = plot_noise)
    
  }
  
)

names(sensitivity_plots_key) <- names(data_labeled)

save_rdata_files(sensitivity_plots_key, folder = "2_code/2_data")

# COMPARISON: LLE & HLLE -------------------------------------------------------

load_rdata_files(data_labeled, folder = "2_code/2_data")
load_rdata_files(sensitivity_landmarks_dt, folder = "2_code/2_data")

# Add world data set

data_labeled$world_data <-
  make_world_data_3d(here("2_code/2_data", "rawdata_world_3d.csv"))

data_unlabeled <- lapply(data_labeled, function(i) {i[, .(x_1, x_2, x_3)]})

true_embeddings <- list(
  incomplete_tire = data_labeled$incomplete_tire[, .(t, s)],
  swiss_roll = data_labeled$swiss_roll[, .(t, s = x_2)],
  world_data = make_world_data_2d(
    here("2_code/2_data", "rawdata_world_2d.csv"))[, .(t = x_1, s = x_2)])

# Perform SSLLE for world data

landmarks_world_ind <- find_landmarks(
  data = data_unlabeled$world_data,
  n_landmarks = 25L,
  n_neighbors = 100L,
  method = "maxmin")

landmarks_world <- true_embedding_world_data[landmarks_world_ind]

new_order_world <- c(
  landmarks_world_ind,
  setdiff(data_labeled$world_data[, .I], landmarks_world_ind))

data_opt <- data.table::copy(sensitivity_landmarks_dt)

data_opt <- lapply(
  data.table::copy(sensitivity_landmarks_dt),
  function(i) i[landmark_method == "maximum_coverage" & n_landmarks == 12L])

data_opt$world_data <- data.table::data.table(
  embedding_result = list(perform_sslle(
    data = data_unlabeled$world_data[new_order_world],
    k_max = k_max,
    prior_points = landmarks_world,
    verbose = TRUE)),
  true_embedding = list(true_embedding_world_data[new_order_world]))

colors <- list(
  incomplete_tire = data_labeled$incomplete_tire[, .(t)],
  swiss_roll = data_labeled$swiss_roll[, .(t)],
  world_data = make_world_data_2d(
    here("2_code/2_data", "rawdata_world_2d.csv"))[, .(t)])

# Compute embeddings and plot

comp_lle <- lapply(
  
  seq_along(data_unlabeled),
  
  function(i) {
    
    if (i < 3L) {
      this_color <- data_opt[[i]]$true_embedding[[1]][, .(t)]
    } else {
      this_color <- make_world_data_2d(
        here("2_code/2_data", "rawdata_world_2d.csv"))[new_order_world][, .(t)]}
    
    plot_sslle <- plot_manifold(
      data = data_opt[[i]]$embedding_result[[1]]$Y,
      intrinsic_coords = this_color,
      point_size = 5L)
    
    res_lle <- dimRed::embed(
      data_unlabeled[[i]][, .(x_1, x_2, x_3)],
      "LLE", 
      ndim = 2L,
      knn = data_opt[[i]]$embedding_result[[1]]$neighborhood_size)
    
    res_hlle <- dimRed::embed(
      data_unlabeled[[i]][, .(x_1, x_2, x_3)],
      "HLLE",
      ndim = 2L,
      knn = data_opt[[i]]$embedding_result[[1]]$neighborhood_size)
    
    res_unsupervised <- list(lle = res_lle, hlle = res_hlle)
    
    plots_unsupervised <- lapply(
      
      seq_along(res_unsupervised),
      
      function(j) {
        
        emb_dt <- data.table::as.data.table(
          res_unsupervised[[j]]@data@data)
        
        plot_manifold(
          data = emb_dt,
          intrinsic_coords = colors[[i]])
        
      }
      
    )
    
    names(plots_unsupervised) <- names(res_unsupervised)
    
    plotly::subplot(
      list(plots_unsupervised$lle, plots_unsupervised$hlle, plot_sslle), 
      nrows = 1L) %>% 
      hide_guides()
    
  }
  
)

names(comp_lle) <- names(data_labeled)

save_rdata_files(comp_lle, folder = "2_code/2_data")
