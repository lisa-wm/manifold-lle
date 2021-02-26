# ------------------------------------------------------------------------------
# VISUALIZATION OF RESULTS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

load_rdata_files(sensitivity_landmarks_dt, "2_code")
load_rdata_files(sensitivity_noise_dt, "2_code")

# VISUALIZATION: AUC_LNK_RNX ---------------------------------------------------

sensitivity_landmarks_plots_quant <- lapply(
  
  seq_along(sensitivity_landmarks_dt),
  
  function(i) {
    
    auc_plot <- plot_quant_res(
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
    
    res_var_plot <- plot_quant_res(
      base_plot = sensitivity_landmarks_dt[[i]][
        , .(landmark_method, n_landmarks, residual_variance)
      ][, res_var_scaled := scale_zero_one(residual_variance)] %>%
        ggplot2::ggplot(aes(
          x = forcats::fct_relevel(
            landmark_method,
            "poor_coverage",
            "random_coverage",
            "maximum_coverage"),
          y = n_landmarks,
          col = res_var_scaled)) + 
        geom_point(size = 10L) + 
        scale_x_discrete(labels = c("poor", "random", "maximum")),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = "residual variance (scaled)",
      x_lab = "coverage",
      low = "green", 
      high = "red")
    
    list(auc_plot = auc_plot, res_var_plot = res_var_plot)
    
    })

names(sensitivity_landmarks_plots_quant) <- names(sensitivity_landmarks_dt)

save_rdata_files(sensitivity_landmarks_plots_quant, folder = "2_code")

# ------------------------------------------------------------------------------

sensitivity_noise_plots_quant <- lapply(
  
  seq_along(sensitivity_noise_dt),
  
  function(i) {
    
    auc_plot <- plot_quant_res(
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
    
    res_var_plot <- plot_quant_res(
      base_plot = sensitivity_noise_dt[[i]][
        , .(noise_level, n_landmarks, residual_variance)
      ][, res_var_scaled := scale_zero_one(residual_variance)] %>%
        ggplot2::ggplot(aes(
          x = noise_level,
          y = n_landmarks,
          col = res_var_scaled)) + 
        geom_point(size = 10L) + 
        scale_x_continuous(breaks = c(0.1, 0.5, 1L, 3L)),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = "residual variance (scaled)",
      x_lab = "noise level",
      low = "green", 
      high = "red")
    
    list(auc_plot = auc_plot, res_var_plot = res_var_plot)
    
  })

names(sensitivity_noise_plots_quant) <- names(sensitivity_noise_dt)

save_rdata_files(sensitivity_noise_plots_quant, folder = "2_code")

# VISUALIZATION: LOW-DIMENSIONAL EMBEDDING -------------------------------------

sensitivity_landmarks_plots_qual <- lapply(
  
  seq_along(sensitivity_landmarks_dt),
  
  function(i) {

    dt <- sensitivity_landmarks_dt[[i]]
    data.table::setorder(dt, -n_landmarks)
    dt_name <- names(sensitivity_landmarks_dt)[i]
    
    plots <- lapply(
      
      seq_len(nrow(dt)),
      function(j) {
        plot_qual_res(
          data = data.table(
            dt[j, ]$embedding_result[[1]]$Y, 
            dt[j, ]$embedding_result[[1]]$X[, .(t, s)]),
          dt_name = names(sensitivity_landmarks_dt)[i],
          annotation_text = sprintf(
            "%s coverage, %d landmarks",
            unlist(stringr::str_split(dt[j, ]$landmark_method, "_"))[1L],
            dt[j, ]$n_landmarks))})
    
    subplot(
      plots,
      nrows = 6L) %>% 
      hide_guides()
    
  }
  
)

names(sensitivity_landmarks_plots_qual) <- names(sensitivity_landmarks_dt)

save_rdata_files(sensitivity_landmarks_plots_qual, folder = "2_code")

# ------------------------------------------------------------------------------

sensitivity_noise_plots_qual <- lapply(
  
  seq_along(sensitivity_noise_dt),
  
  function(i) {
    
    dt <- sensitivity_noise_dt[[i]]
    data.table::setorder(dt, -n_landmarks)
    dt_name <- names(sensitivity_noise_dt)[i]
    
    plots <- lapply(
      
      seq_len(nrow(dt)),
      function(j) {
        plot_qual_res(
          data = data.table(
            dt[j, ]$embedding_result[[1]]$Y, 
            dt[j, ]$embedding_result[[1]]$X[, .(t, s)]),
          dt_name = names(sensitivity_noise_dt)[i],
          annotation_text = sprintf(
            "noise %.1f, %d landmarks",
            dt[j, ]$noise_level,
            dt[j, ]$n_landmarks))})
    
    subplot(
      plots,
      nrows = 7L) %>% 
      hide_guides()
    
  }
  
)

names(sensitivity_noise_plots_qual) <- names(sensitivity_noise_dt)

save_rdata_files(sensitivity_noise_plots_qual, folder = "2_code")

# VISUALIZATION: KEY VARIATION -------------------------------------------------

load_rdata_files(data_labeled, folder = "2_code")

# ------------------------------------------------------------------------------

sensitivity_landmarks_plots_key_variation <- lapply(
  
  seq_along(sensitivity_landmarks_dt), 
  
  function(i) {
    
    base_plot <- plot_manifold(
      data_labeled[[i]][, .(t, s, t, t)],
      dim = 2L,
      point_size_1_2_d = 5L) 
    
    landmarks_poor <- 
      data.table::as.data.table(sensitivity_landmarks_dt[[i]][
        landmark_method == "poor_coverage" & n_landmarks == 12L]$landmarks)
    
    landmarks_random <- 
      data.table::as.data.table(sensitivity_landmarks_dt[[i]][
        landmark_method == "random_coverage" & n_landmarks == 12L]$landmarks)
    
    landmarks_maximum <- 
      data.table::as.data.table(sensitivity_landmarks_dt[[i]][
        landmark_method == "maximum_coverage" & n_landmarks == 12L]$landmarks)
    
    make_annotations <- function(base_plot, annotation_text) {
      
      base_plot %>% 
        plotly::layout(annotations = list(
          text = annotation_text,
          xref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = -0.5,
          showarrow = FALSE))
      
    }
    
    plot_poor <- #make_annotations(
      base_plot %>%
        plotly::add_trace(
          x = ~ landmarks_poor$y_1,
          y = ~ landmarks_poor$y_2,
          color = ~ 1L,
          type = "scatter",
          mode = "markers",
          marker = list(color = "black", size = 10L))#,
      # annotation_text = "poor coverage") 
    
    plot_random <- #make_annotations(
      base_plot %>%
        plotly::add_trace(
          x = ~ landmarks_random$y_1,
          y = ~ landmarks_random$y_2,
          color = ~ 1L,
          type = "scatter",
          mode = "markers",
          marker = list(color = "black", size = 10L, symbol = "x"))#,
      # annotation_text = "random coverage")
    
    plot_maximum <- #make_annotations(
      base_plot %>%
        plotly::add_trace(
          x = ~ landmarks_maximum$y_1,
          y = ~ landmarks_maximum$y_2,
          color = ~ 1L,
          type = "scatter",
          mode = "markers",
          marker = list(color = "black", size = 10L, symbol = "star")) #,
      # annotation_text = "maximum coverage")
    
      plotly::subplot(list(
        plot_poor = plot_poor, 
        plot_random = plot_random, 
        plot_maximum = plot_maximum),
        nrows = 1L) %>% 
        hide_guides()
    
  }
  
)

names(sensitivity_landmarks_plots_key_variation) <- 
  names(sensitivity_landmarks_dt)

save_rdata_files(sensitivity_landmarks_plots_key_variation, folder = "2_code")

# ------------------------------------------------------------------------------

sensitivity_noise_plots_key_variation <- lapply(
  
  seq_along(sensitivity_noise_dt), 
  
  function(i) {
    
    landmarks <- data.table::as.data.table(sensitivity_noise_dt[[i]][
      noise_level == 0.1 & n_landmarks == 12L]$landmarks)
    
    sd_s = sd(data_labeled[[i]]$s)
    sd_t = sd(data_labeled[[i]]$t)
    
    base_plot <- plot_manifold(
      data_labeled[[i]][, .(t, s, t, t)],
      dim = 2L,
      point_size_1_2_d = 5L) %>% 
      add_trace(
        x = ~ landmarks$t,
        y = ~ landmarks$s,
        color = ~ 1L,
        type = "scatter",
        mode = "markers",
        marker = list(color = "black", size = 10L)) %>% 
      hide_guides()
    
    noise_plots <- lapply(
      
      seq_along(unique(sensitivity_noise_dt[[i]]$noise_level)), 
      
      function(j) {
        
        noise_level <- unique(sensitivity_noise_dt[[i]]$noise_level)[j]
        
        base_plot %>% 
          layout(shapes = list(
            type = "circle",
            xref = "x",
            yref = "y",
            x0 = landmarks[12L]$t - noise_level * sd_t,
            x1 = landmarks[12L]$t + noise_level * sd_t,
            y0 = landmarks[12L]$s - noise_level * sd_s,
            y1 = landmarks[12L]$s + noise_level * sd_s,
            fillcolor = "black",
            opacity = 0.4,
            line = list(color = "black")
          ))
        
      })
    
    subplot(
      noise_plots, nrows = 1L) %>%
      hide_guides()
    
  }
  
)

names(sensitivity_noise_plots_key_variation) <- 
  names(sensitivity_noise_dt)

save_rdata_files(sensitivity_noise_plots_key_variation, folder = "2_code")

# COMPARISON: LLE & HLLE -------------------------------------------------------

load_rdata_files(data_labeled, folder = "2_code")

data_unlabeled <- lapply(data_labeled, function(i) {i[, .(x_1, x_2, x_3)]})

comp_lle <- lapply(
  
  seq_along(data_unlabeled),
  
  function(i) {
    
    res_lle <- dimRed::embed(
      data_unlabeled[[i]][, .(x_1, x_2, x_3)],
      "LLE", 
      ndim = 2L,
      knn = data_opt$embedding_result[[1]]$neighborhood_size)
    
    res_hlle <- dimRed::embed(
      data_unlabeled[[i]][, .(x_1, x_2, x_3)],
      "HLLE", 
      ndim = 2L,
      knn = data_opt$embedding_result[[1]]$neighborhood_size)
    
    res <- list(lle = res_lle, hlle = res_hlle)
    
    plots <- lapply(
      
      seq_along(res),
      
      function(j) {
        
        emb_dt <- data.table::as.data.table(
          res[[j]]@data@data)
        
        plot_manifold(
          data.table::data.table(
            emb_dt,
            data_labeled[[i]][, .(t, s)]),
          dim = 2L)
        
      }
      
    )
    
    names(plots) <- names(res)
    
    data_opt <- sensitivity_landmarks_dt[[i]][
      landmark_method == "maximum_coverage" & n_landmarks == 12L]
    
    plot_sslle <- plot_manifold(
      data.table::data.table(
        data_opt$embedding_result[[1]]$Y,
        data_opt$embedding_result[[1]]$X[, .(t, s)]),
      dim = 2L)
    
    plotly::subplot(list(plots$lle, plots$hlle, plot_sslle), nrows = 1L) %>% 
      hide_guides()
    
  }
  
)

names(comp_lle) <- names(data_labeled)

save_rdata_files(comp_lle, folder = "2_code")
