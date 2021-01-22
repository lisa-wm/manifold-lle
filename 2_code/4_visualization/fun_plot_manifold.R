# ------------------------------------------------------------------------------
# MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

plot_manifold <- function(data, dim, n_colors = 10) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_int(dim, lower = 1L, upper = 3L)
  checkmate::assert_count(n_colors)
  
  if (ncol(data) != dim + 1L) 
    {stop(sprintf("data must contain %d columns plus one for coloring", dim))}
  
  colnames <- c(c("x", "y", "z")[1:dim], "t")
  setnames(data, colnames)

  # Create rainbow color palette, granularity depending on n_colors
  
  my_palette <- rainbow(n_colors)
  
  if (dim <= 2L) {
    
    # Remove coordinate system
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    
    if (dim == 1L) {
      my_plot <- plotly::plot_ly(data, x = ~ t, y = 0, color = ~ t)
    } else {
      my_plot <- plotly::plot_ly(data, x = ~ x, y = ~ y, color = ~ t)
    }
    
    my_plot <- my_plot %>% 
      add_trace(
        type = "scatter",
        mode = "markers",
        colors = my_palette
      ) %>% 
      hide_colorbar() %>%
      layout(xaxis = ax, yaxis = ax)
    
  }
  
  if (dim == 3L) {
    
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
    
    my_plot <-plotly::plot_ly(
      data, 
      x = ~ x, 
      y = ~ y, 
      z = ~ z, 
      color = ~ t) %>% 
      add_trace(
        type = "scatter3d",
        mode = "markers",
        colors = my_palette
      ) %>% 
      hide_colorbar() %>% 
      layout(scene = scene)

  }
  
  my_plot
  
}
