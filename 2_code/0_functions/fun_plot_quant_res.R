# ------------------------------------------------------------------------------
# PLOT QUANTITATIVE RESULTS
# ------------------------------------------------------------------------------

# Purpose: make grid plot for quantitative results of sensitivity analysis

plot_quant_res <- function(base_plot,
                           dt_name,
                           legend_title,
                           x_lab,
                           low = "red",
                           high = "green") {
  
  base_plot +
    scale_color_gradient(
      low = low,  
      high = high,
      name = legend_title,
      limits = c(0L, 1L)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    scale_y_continuous(breaks = seq(2L, 12L, by = 2L)) +
    xlab(as.character(x_lab)) +
    ylab("number of landmarks") +
    ggtitle(sprintf("Data: %s", dt_name))
  
}
