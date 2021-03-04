# ------------------------------------------------------------------------------
# MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

plot_manifold <- function(data,
                          intrinsic_coords,
                          n_colors = 10, 
                          coord_syst = FALSE,
                          camera_eye = list(
                            x = 1.5, 
                            y = -1.5, 
                            z = 0.75),
                          title = NULL, 
                          point_size = 10L) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_data_table(intrinsic_coords)
  checkmate::assert_count(n_colors)
  
  dt <- data.table::copy(data)
  dim <- ncol(dt)

  data.table::setnames(dt, c("x", "y", "z")[1:dim])
  data.table::setnames(intrinsic_coords, "t")
  
  dt_plot <- cbind(dt, intrinsic_coords)

  # Create rainbow color palette, granularity depending on n_colors
  
  my_palette <- grDevices::rainbow(n_colors)
  
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
      my_plot <- plotly::plot_ly(dt_plot, x = ~ x, y = 0, color = ~ t)
    } else {
      my_plot <- plotly::plot_ly(dt_plot, x = ~ x, y = ~ y, color = ~ t)}
    
    my_plot <- my_plot %>% 
      add_trace(
        type = "scatter",
        mode = "markers",
        colors = my_palette,
        marker = list(size = point_size)
      ) %>% 
      hide_colorbar() %>%
      layout(xaxis = ax, yaxis = ax, title = title)
    
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
      dt_plot, 
      x = ~ x, 
      y = ~ y, 
      z = ~ z, 
      color = ~ t) %>% 
      add_trace(
        type = "scatter3d",
        mode = "markers",
        colors = my_palette,
        marker = list(size = 0.5 * point_size)
      ) %>% 
      hide_colorbar() %>% 
      layout(scene = scene, title = title)

  }
  
  my_plot
  
}
