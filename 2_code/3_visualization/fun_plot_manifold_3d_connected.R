# ------------------------------------------------------------------------------
# 3D MANIFOLD VISUALIZATION
# ------------------------------------------------------------------------------

#' Plot rainbow-colored manifolds in 3D
#'
#' @param data Data table object containing three columns representing points'
#' coordinates in R3 (named x, y, z) and one column representing points' main
#' dimension on the manifold (named t; used for coloring)
#' @param k Number of nearest neighbors to be found
#' @return Plotly object

# FIXME Make available for k > 1
# FIXME Avoid large jump in the end

plot_manifold_3d_connected <- function(data, k = 2L) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  stopifnot(names(data) %in% c("x", "y", "z", "t"))
  
  k_neighborhoods <- kknn::kknn(
    t ~ .,
    data, # train
    data, # test
    k = k + 1
  )$C
  
  k_neighborhoods <- as.data.table(k_neighborhoods)
  setnames(k_neighborhoods, c("x", paste0("neighbor_", c(1:k))))
  
  neighborhood_data <- lapply(
    seq_len(nrow(data)), 
    function(i) {
      bind_rows(data[i, ], data[as.numeric(k_neighborhoods[i, 2:(k + 1)]), ])})
  
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
  # neigborhoods are established, only within)
  
  for (i in seq_len(nrow(data))) {

    neighborhood_graph <- neighborhood_graph %>%
      add_trace(
        x = neighborhood_data[[i]]$x,
        y = neighborhood_data[[i]]$y,
        z = neighborhood_data[[i]]$z,
        type = "scatter3d",
        mode = "segments",
        line = list(size = 3L, color = "gray")
      )

  }
  
  neighborhood_graph
  
}
