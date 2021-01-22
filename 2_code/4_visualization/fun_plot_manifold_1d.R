# ------------------------------------------------------------------------------
# 1D MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

#' Plot rainbow-colored manifolds in 1D
#'
#' @param data Data table object containing a single column representing points'
#' main dimension on the manifold (named t; used for coloring)
#' @param n_colors Number of rainbow columns to be displayed
#' @return Plotly object

plot_manifold_1d <- function(data, n_colors = 10) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_count(n_colors)
  stopifnot(names(data) %in% c("t"))
  
  # Remove coordinate system
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  # Create rainbow color palette, granularity depending on n_colors
  
  my_palette <- rainbow(n_colors)
  
  # Make 3D scatterplot
  
  plotly::plot_ly(data, x = ~ t, y = 0, color = ~ t) %>% 
    add_trace(
      type = "scatter",
      mode = "markers",
      colors = my_palette
    ) %>% 
    hide_colorbar() %>%
    layout(xaxis = ax, yaxis = ax)
  
}
