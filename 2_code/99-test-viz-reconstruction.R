vertices_2d <- data.table(
  x = c(1.2, 1.5, 2, 1, 1.8),
  y = c(1, 2, 2.2, 1.6, 2),
  weights = c(0.2, 0.1, 0.2, 0.3, 0.2))

# neighborhood_2d <- as.data.table(dplyr::bind_rows(
#   vertices_2d,
#   c(
#     x = sum(vertices_2d$x * vertices_2d$weights),
#     y = sum(vertices_2d$y * vertices_2d$weights),
#     weights = 0,
#     is_center = 1L)))

center_2d <- data.table(
  x = sum(vertices_2d$x * vertices_2d$weights),
  y = sum(vertices_2d$y * vertices_2d$weights))

vertices_3d <- data.table(
  x = c(1.2, 1.5, 2, 1, 1.8),
  y = c(1, 2, 2.2, 1.6, 2),
  z = c(0.5, 1, 1, 0.8, 0.5),
  weights = c(0.2, 0.1, 0.2, 0.3, 0.2))

# neighborhood_3d <- as.data.table(dplyr::bind_rows(
#   vertices_3d,
#   c(
#     x = sum(vertices_3d$x * vertices_3d$weights),
#     y = sum(vertices_3d$y * vertices_3d$weights),
#     z = sum(vertices_3d$z * vertices_3d$weights),
#     weights = 0,
#     is_center = 1L)))

center_3d <- data.table(
  x = sum(vertices_3d$x * vertices_3d$weights),
  y = sum(vertices_3d$y * vertices_3d$weights),
  z = sum(vertices_3d$z * vertices_3d$weights))

edges_3d <- lapply(
  seq_len(nrow(vertices_3d)), 
  function(i) {rbind(vertices_3d[i, .(x, y, z)], center_3d)})

scene <- list(
  camera = list(eye = list(
    x = 1.5 * 2, 
    y = -0.5 * 2, 
    z = 0.25 * 2)
  ),
  xaxis = list(showticklabels = FALSE),
  yaxis = list(showticklabels = FALSE),
  zaxis = list(showticklabels = FALSE)
)

neighborhood_graph <- plotly::plot_ly(
  vertices_3d, 
  x = ~ x, 
  y = ~ y, 
  z = ~ z) %>% 
  add_trace(
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 20L, color = "gray")
  )  %>% 
  hide_colorbar() %>% 
  hide_guides() %>% 
  layout(scene = scene)

for (i in seq_along(edges_3d)) {
  
  neighborhood_graph <- neighborhood_graph %>%
    add_trace(
      x = edges_3d[[i]]$x,
      y = edges_3d[[i]]$y,
      z = edges_3d[[i]]$z,
      type = "scatter3d",
      mode = "segments",
      line = list(size = 10L, color = "gray"))
  
}

neighborhood_graph <- neighborhood_graph %>%
  add_trace(
    x = ~ center_3d$x,
    y = ~ center_3d$y,
    z = ~ center_3d$z,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 20L, color = "blue"))
