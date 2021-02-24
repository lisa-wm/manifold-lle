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
    
    auc_plot <-  sensitivity_landmarks_dt[[i]][
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
      scale_color_gradient(
        low = "red", 
        high = "green",
        name = expression(AUC(R[NX]) ~ " (scaled)"),
        limits = c(0L, 1L)
        # limits = c(
        #   min(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx),
        #   max(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx))
      ) +
      theme_bw() +
      theme(text = element_text(size = 20)) +
      scale_x_discrete(labels = c("poor", "random", "maximum")) +
      scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
      xlab("coverage") +
      ylab("number of landmarks") +
      ggtitle(sprintf(
        "Data: %s",
        stringr::str_replace_all(
          names(sensitivity_landmarks_dt[i]), "_", " ")))
    
    res_var_plot <- sensitivity_landmarks_dt[[i]][
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
      scale_color_gradient(
        low = "green", 
        high = "red",
        name = "residual variance (scaled)",
        limits = c(0L, 1L)) +
      theme_bw() +
      theme(text = element_text(size = 20L)) +
      scale_x_discrete(labels = c("poor", "random", "maximum")) +
      scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
      xlab("coverage") +
      ylab("number of landmarks") +
      ggtitle(sprintf(
        "Data: %s",
        stringr::str_replace_all(
          names(sensitivity_landmarks_dt[i]), "_", " ")))

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
    
    auc_plot <-  sensitivity_noise_pp_dt[[i]][
      , .(noise_level, n_landmarks, auc_lnk_rnx)
    ][, auc_scaled := scale_01(auc_lnk_rnx)] %>% 
      ggplot2::ggplot(aes(
        x = noise_level, 
        y = n_landmarks, 
        col = auc_scaled)) +
      geom_point(size = 10L) +
      scale_color_gradient(
        low = "red", 
        high = "green",
        name = expression(AUC(R[NX]) ~ " (scaled)"),
        limits = c(0L, 1L)
        # limits = c(
        #   min(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx),
        #   max(do.call(rbind, sensitivity_landmarks_dt)$auc_lnk_rnx))
      ) +
      theme_bw() +
      theme(text = element_text(size = 20L)) +
      scale_x_continuous(breaks = c(0.1, 0.5, 1L, 3L, 5L)) +
      scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
      xlab("noise level") +
      ylab("number of landmarks") +
      ggtitle(sprintf(
        "Data: %s",
        stringr::str_replace_all(
          names(sensitivity_landmarks_dt[i]), "_", " ")))
    
    # res_var_plot <- sensitivity_landmarks_dt[[i]][
    #   , .(landmark_method, n_landmarks, residual_variance, auc_lnk_rnx)
    # ][, res_var_scaled := scale_01(residual_variance)] %>% 
    #   ggplot2::ggplot(aes(
    #     x = forcats::fct_relevel(
    #       landmark_method, 
    #       "poor_coverage",
    #       "random_coverage",
    #       "maximum_coverage"), 
    #     y = n_landmarks, 
    #     col = res_var_scaled)) +
    #   geom_point(size = 5L) +
    #   scale_color_gradient(
    #     low = "green", 
    #     high = "red",
    #     name = "residual variance (scaled)",
    #     limits = c(0L, 1L)) +
    #   scale_x_discrete(labels = c("poor", "random", "maximum")) +
    #   scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
    #   xlab("coverage") +
    #   ylab("number of landmarks") +
    #   ggtitle(sprintf(
    #     "Data: %s",
    #     stringr::str_replace_all(
    #       names(sensitivity_landmarks_dt[i]), "_", " ")))
    
    list(auc_plot = auc_plot
         # , res_var_plot = res_var_plot
         )
    
  })

names(sensitivity_noise_plots_quant) <- names(sensitivity_noise_pp_dt)

save_rdata_files(sensitivity_noise_plots_quant, folder = "2_code")

gridExtra::grid.arrange(
  sensitivity_noise_plots_quant$swiss_roll$auc_plot,
  sensitivity_noise_plots_quant$incomplete_tire$auc_plot,
  ncol = 2L)

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

names(sensitivity_landmarks_plots_qual) <- names(sensitivity_landmarks_dt)

save_rdata_files(sensitivity_landmarks_plots_qual, folder = "2_code")

# ------------------------------------------------------------------------------

# sensitivity_noise_plots_qual <- lapply(
#   
#   seq_along(sensitivity_noise_dt),
#   
#   function(i) {
#     
#     dt <- sensitivity_noise_dt[[i]]
#     data.table::setorder(dt, confidence_param, noise_level)
#     dt_name <- names(sensitivity_noise_dt)[i]
#     
#     plots <- lapply(
#       seq_len(nrow(dt)),
#       function(j) {
#         plot_manifold(
#           data.table(
#             dt[j, ]$embedding_result[[1]]$Y, 
#             dt[j, ]$embedding_result[[1]]$X[, .(t, s)]), 
#           dim = 2L,
#           title = sprintf(
#             "Data: %s", 
#             unlist(stringr::str_replace(dt_name, "_", " ")))) %>% 
#           layout(annotations = list(
#             text = sprintf(
#               "noise %.3f, confidence %.3f",
#               dt[j, ]$noise_level,
#               dt[j, ]$confidence_param),
#             xref = "paper",
#             yref = "paper",
#             yanchor = "bottom",
#             xanchor = "center",
#             align = "center",
#             x = 0.5,
#             y = -0.25,
#             showarrow = FALSE))
#         
#       })
#     
#     subplot(
#       plots,
#       nrows = 7L) %>% 
#       hide_guides()
#     
#   }
#   
# )
# 
# save_rdata_files(sensitivity_noise_plots_qual, folder = "2_code")

# ------------------------------------------------------------------------------

sensitivity_noise_pp_plots_qual <- lapply(
  
  seq_along(sensitivity_noise_pp_dt),
  
  function(i) {
    
    dt <- sensitivity_noise_pp_dt[[i]]
    data.table::setorder(dt, -n_landmarks)
    dt_name <- names(sensitivity_noise_pp_dt)[i]
    
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
              "noise %.1f, %d landmarks",
              dt[j, ]$noise_level,
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
      nrows = 7L) %>% 
      hide_guides()
    
  }
  
)

names(sensitivity_noise_pp_plots_qual) <- names(sensitivity_noise_pp_dt)

save_rdata_files(sensitivity_noise_pp_plots_qual, folder = "2_code")

