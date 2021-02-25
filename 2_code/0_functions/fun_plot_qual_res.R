# ------------------------------------------------------------------------------
# PLOT QUALITATIVE RESULTS
# ------------------------------------------------------------------------------

# Purpose: make plot for qualitative results of sensitivity analysis

plot_qual_res <- function(data, dt_name, annotation_text) {
      
  plot_manifold(
    data, 
    dim = 2L,
    # title = sprintf(
    #   "Data: %s", 
    #   unlist(stringr::str_replace(dt_name, "_", " "))),
    point_size_1_2_d = 3L) %>% 
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
