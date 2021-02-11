# ------------------------------------------------------------------------------
# MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

plot_manifold <- function(data, 
                          dim, 
                          n_colors = 10, 
                          coord_syst = FALSE,
                          camera_eye = list(
                            x = 1.5, 
                            y = -1.5, 
                            z = 0.75)) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_int(dim, lower = 1L, upper = 3L)
  checkmate::assert_count(n_colors)
  
  if (ncol(data) != dim + 2L)
    {stop(sprintf("data must contain %d columns plus two for coloring", dim))}
  
  colnames <- c(c("x", "y", "z")[1:dim], "t", "s")
  setnames(data, colnames)

  # Create rainbow color palette, granularity depending on n_colors
  
  my_palette <- rainbow(n_colors)
  
  if (dim <= 2L) {
    
    # Remove coordinate system if desired
    
    if (coord_syst) {
      ax <- list(title = "", showticklabels = FALSE)
    } else {
      
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE)
      
    }
    
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
    
    if (coord_syst) {
      
      ax <- list(
        showticklabels = FALSE, 
        showline = TRUE, 
        showgrid = FALSE, 
        title = "")
      
      scene <- list(
        camera = list(eye = camera_eye),
        xaxis = ax,
        yaxis = ax,
        zaxis = ax)
      
    } else {
      
      scene <- list(
        camera = list(eye = camera_eye),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE))
      
    }
    
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
