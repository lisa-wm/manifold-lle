# ------------------------------------------------------------------------------
# VISUALIZATION OF RESULTS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

load_rdata_files(sensitivity_landmarks_dt, "2_code")
load_rdata_files(sensitivity_noise_pp_dt, "2_code")

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

gridExtra::grid.arrange(
  sensitivity_landmarks_plots_quant$swiss_roll$auc_plot,
  sensitivity_landmarks_plots_quant$incomplete_tire$auc_plot,
  ncol = 2L)

# ------------------------------------------------------------------------------

sensitivity_noise_plots_quant <- lapply(
  
  seq_along(sensitivity_noise_pp_dt),
  
  function(i) {
    
    auc_plot <- plot_quant_res(
      base_plot = sensitivity_noise_pp_dt[[i]][
        , .(noise_level, n_landmarks, auc_lnk_rnx)
      ][, auc_scaled := scale_zero_one(auc_lnk_rnx)] %>%
        ggplot2::ggplot(aes(
          x = noise_level,
          y = n_landmarks,
          col = auc_scaled)) + 
        geom_point(size = 10L) + 
        scale_x_continuous(breaks = c(0.1, 0.5, 1L, 3L, 5L)),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = expression(AUC(R[NX]) ~ " (scaled)"),
      x_lab = "noise level")
    
    res_var_plot <- plot_quant_res(
      base_plot = sensitivity_noise_pp_dt[[i]][
        , .(noise_level, n_landmarks, residual_variance)
      ][, res_var_scaled := scale_zero_one(residual_variance)] %>%
        ggplot2::ggplot(aes(
          x = noise_level,
          y = n_landmarks,
          col = res_var_scaled)) + 
        geom_point(size = 10L) + 
        scale_x_continuous(breaks = c(0.1, 0.5, 1L, 3L, 5L)),
      dt_name = stringr::str_replace_all(
        names(sensitivity_landmarks_dt[i]), "_", " "),
      legend_title = "residual variance (scaled)",
      x_lab = "noise level",
      low = "green", 
      high = "red")
    
    list(auc_plot = auc_plot, res_var_plot = res_var_plot)
    
  })

names(sensitivity_noise_plots_quant) <- names(sensitivity_noise_pp_dt)

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
          dt_name = names(sensitivity_noise_pp_dt)[i],
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
  
  seq_along(sensitivity_noise_pp_dt),
  
  function(i) {
    
    dt <- sensitivity_noise_pp_dt[[i]]
    data.table::setorder(dt, -n_landmarks)
    dt_name <- names(sensitivity_noise_pp_dt)[i]
    
    plots <- lapply(
      
      seq_len(nrow(dt)),
      function(j) {
        plot_qual_res(
          data = data.table(
            dt[j, ]$embedding_result[[1]]$Y, 
            dt[j, ]$embedding_result[[1]]$X[, .(t, s)]),
          dt_name = names(sensitivity_noise_pp_dt)[i],
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

names(sensitivity_noise_plots_qual) <- names(sensitivity_noise_pp_dt)

save_rdata_files(sensitivity_noise_plots_qual, folder = "2_code")

