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

# FIXME Make available for k > 1
# FIXME Avoid large jump in the end

plot_manifold_3d_connected <- function(data, n_colors = 10) {
  
  # Perform basic input checks
  
  checkmate::assert_data_table(data)
  checkmate::assert_count(n_colors)
  stopifnot(names(data) %in% c("x", "y", "z", "t"))
  
  data_ordered_t <- setorder(copy(data), t)
  data_ordered_t[, id := .I]
  dist_mat <- dist(data_ordered_t[, .(x, y, z)], method = "euclidean")
  x <- data_ordered_t[id == 1L]
  used_points <- c(x$id)
  
  while (length(used_points) < nrow(data)) {
    
    dist_vec_x <- as.matrix(dist_mat)[x$id, ]
    neighbors_x <- (order(dist_vec_x)[1:nrow(data)])[-1]
    new_neighbors_x <- setdiff(neighbors_x, used_points)
    next_id <- new_neighbors_x[1]
    used_points <- c(used_points, next_id)
    x <- data_ordered_t[next_id, ]
    
  }
  
  data_ordered_eucl <- data_ordered_t[match(used_points, data_ordered_t$id), ]
  
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

  plotly::plot_ly(
    data_ordered_eucl, 
    x = ~ x, 
    y = ~ y, 
    z = ~ z,
    marker = list(size = 3L, color = "gray"),
    line = list(size = 3L, color = "gray")) %>% 
    add_trace(
      type = "scatter3d",
      mode = "lines+markers"
    ) %>%
    hide_colorbar() %>% 
    layout(scene = scene)
  
}

plot_manifold_3d_connected(make_s_curve(n_points = 100L)) 
