# ------------------------------------------------------------------------------
# 3D MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

#' Plot rainbow-colored manifolds in 3D
#'
#' @param data Data table object containing three columns representing points'
#' coordinates in R3 (named x, y, z) and one column representing points' main
#' dimension on the manifold (named t; used for coloring)
#' @param n_colors Number of rainbow columns to be displayed
#' @return Plotly object

# TODO Check whether internal call of plotly to arrange_, which is deprecated
# as of dplyr 0.7, can be avoided

plot_manifold_3d <- function(data, n_colors = 10) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_count(n_colors)
  stopifnot(names(data) %in% c("x", "y", "z", "t"))
  
  # Define camera perspective and axis titles
  
  scene <- list(
    camera = list(eye = list(
      x = 1.5, 
      y = -1.5, 
      z = 0.75)
    ),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    zaxis = list(visible = FALSE)
  )
  
  # Create rainbow color palette, granularity depending on n_colors
  
  my_palette <- rainbow(n_colors)
  
  # Make 3D scatterplot
  
  plotly::plot_ly(data, x = ~ x, y = ~ y, z = ~ z, color = ~ t) %>% 
    add_trace(
      type = "scatter3d",
      mode = "markers",
      colors = my_palette
    ) %>% 
    hide_colorbar() %>% 
    layout(scene = scene)
  
}
