# ------------------------------------------------------------------------------
# VISUALIZATION OF RESULTS
# ------------------------------------------------------------------------------

# DATA -------------------------------------------------------------------------

load_rdata_files(sensitivity_landmarks_dt, "2_code")

# VISUALIZATION: AUC_LNK_RNX & RESIDUAL VARIANCE -------------------------------

# TODO find way to scale color limits properly

sensitivity_landmarks_plots_quant <- lapply(
  
  seq_along(sensitivity_landmarks_dt),
  
  function(i) {
    
    list(
      
      auc_plot = sensitivity_landmarks_dt[[i]][
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
        scale_color_gradient(
          low = "red", 
          high = "green"
          # , 
          # limits = c(
          #   min(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx),
          #   max(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx))
        ) +
        ylab("number of landmarks") +
        xlab("") +
        ggtitle(sprintf(
          "Data: %s", 
          stringr::str_replace_all(
            names(sensitivity_landmarks_dt[i]), "_", " "))),
      
      res_var_plot = sensitivity_landmarks_dt[[i]][
        , .(landmark_method, n_landmarks, residual_variance, auc_lnk_rnx)] %>% 
        ggplot2::ggplot(aes(
          x = forcats::fct_relevel(
            landmark_method, 
            "poor_coverage",
            "random_coverage",
            "optimal_coverage"), 
          y = n_landmarks, 
          col = residual_variance)) +
        geom_point(size = 5L) +
        scale_color_gradient(
          low = "green", 
          high = "red"
          # , 
          # limits = c(
          #   min(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx),
          #   max(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx))
        ) +
        ylab("number of landmarks") +
        xlab("") +
        ggtitle(sprintf(
          "Data: %s", 
          stringr::str_replace_all(
            names(sensitivity_landmarks_dt[i]), "_", " "))))})

# VISUALIZATION: LOW-DIMENSIONAL EMBEDDING -------------------------------------

sensitivity_landmarks_plots_qual <- lapply(
  
  seq_along(sensitivity_landmarks_dt),
  
  function(i) {
    
    dt <- sensitivity_landmarks_dt[[i]]
    dt_name <- names(sensitivity_landmarks_dt)[i]
    
    plots <- lapply(
      seq_len(nrow(dt)),
      function(j) {
        plot_manifold(
          data.table(
            dt[j, ]$embedding_result[[1]]$Y, 
            dt[j, ]$embedding_result[[1]]$X[, .(t, s)]), 
          dim = 2L,
          title = sprintf(
            "Data: %s", 
            unlist(stringr::str_replace(dt_name, "_", " ")))) %>% 
          layout(annotations = list(
            text = sprintf(
              "%s coverage, %d landmarks",
              unlist(stringr::str_split(dt[j, ]$landmark_method, "_"))[1L],
              dt[j, ]$n_landmarks),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = -0.25,
            showarrow = FALSE))
          
      })
    
    subplot(
      plots,
      nrows = 6L) %>% 
      hide_guides()
    
  }
  
)

save_rdata_files(sensitivity_landmarks_plots_qual, folder = "2_code")
