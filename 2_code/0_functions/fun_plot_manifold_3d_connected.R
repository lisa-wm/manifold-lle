# ------------------------------------------------------------------------------
# 3D MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

# FIXME Avoid large jump in the end

plot_manifold_3d_connected <- function(data, k = 2L) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  stopifnot(names(data) %in% c("x", "y", "z", "t", "s"))
  
  k_neighborhoods <- kknn::kknn(
    t ~ .,
    train = data,
    test = data,
    k = k + 1
  )$C
  
  k_neighborhoods <- as.data.table(k_neighborhoods)
  setnames(k_neighborhoods, c("x", sprintf("neighbor_%d", seq_len(k))))

  neighborhood_data <- lapply(
    seq_len(nrow(k_neighborhoods)),
    function(i) {
      lapply(
        seq_len(k),
        function(j) {
          rbind(
            data[i, ], 
            data[as.numeric(k_neighborhoods[i, j + 1, with = FALSE]), ])})})

  scene <- list(
    camera = list(eye = list(
      x = 1.5 * 2, 
      y = -1.5 * 2, 
      z = 0.75 * 2)
    ),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    zaxis = list(visible = FALSE)
  )
  
  # Make 3D scatterplot
  
  neighborhood_graph <- plotly::plot_ly(
    data, 
    x = ~ x, 
    y = ~ y, 
    z = ~ z) %>% 
    add_trace(
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 3L, color = "gray")
    )  %>% 
    hide_colorbar() %>% 
    hide_guides() %>% 
    layout(scene = scene)
  
  # Add edges in a sequential manner (necessary so that no connections between 
  # neighborhoods are established, only within)
  
  for (i in seq_len(nrow(data))) {
    
    for (k in seq_len(k)) {
      
      neighborhood_graph <- neighborhood_graph %>%
        add_trace(
          x = neighborhood_data[[i]][[k]]$x,
          y = neighborhood_data[[i]][[k]]$y,
          z = neighborhood_data[[i]][[k]]$z,
          type = "scatter3d",
          mode = "segments",
          line = list(size = 3L, color = "gray")
        )
      
    }
    
  }
  
  neighborhood_graph
  
}
