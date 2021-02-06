x <- c(1.2, 1.5, 2, 1, 1.8)
y <- c(1, 2, 2.2, 1.6, 1.4)
z <- c(0.5, 1, 1, 0.8, 0.5)
weights <- c(0.2, 0.1, 0.2, 0.3, 0.2)

vertices_2d <- data.table(x, y, weights)
center_2d <- data.table(
  x = sum(vertices_2d$x * vertices_2d$weights),
  y = sum(vertices_2d$y * vertices_2d$weights))

edges_2d <- lapply(
  seq_len(nrow(vertices_3d)), 
  function(i) {rbind(vertices_2d[i, .(x, y)], center_2d)})

ax <- list(showticklabels = FALSE, showline = TRUE, title = "")

neighborhood_graph_2d <- plotly::plotly_empty() %>% 
  layout(
    xaxis = ax, 
    yaxis = ax)

for (i in seq_along(edges_2d)) {
  
  neighborhood_graph_2d <- neighborhood_graph_2d %>%
    add_trace(
      x = ~ edges_2d[[i]]$x,
      y = ~ edges_2d[[i]]$y,
      # colors = "black",
      # colors = rep("gray", 2),
      # color = ~ 1:2,
      type = "scatter",
      mode = "segments",
      line = list(size = 10L, color = "gray")
    ) %>% 
    hide_guides()
  
}

for (i in seq_len(nrow(vertices_2d))) {

  neighborhood_graph_2d <- neighborhood_graph_2d %>%
    add_trace(
      x = ~ edges_2d[[i]]$x,
      y = ~ edges_2d[[i]]$y,
      # colors = "black",
      # colors = rep("gray", 2),
      # color = ~ 1:2,
      type = "scatter",
      mode = "markers",
      marker = list(size = 20L, color = rainbow(nrow(vertices_2d))[i])
    ) %>% 
    hide_guides()
    
}

neighborhood_graph_2d <- neighborhood_graph_2d %>%
  add_trace(
    x = ~ center_2d$x,
    y = ~ center_2d$y,
    # colors = "black",
    type = "scatter",
    mode = "markers",
    marker = list(size = 20L, color = "gray")) %>% 
  hide_colorbar()

neighborhood_graph_2d

neighborhood_graph_2d <- plotly::plot_ly(
  vertices_2d, 
  x = ~ x, 
  y = ~ y
  # ,
  # color = ~ seq_len(nrow(vertices_2d))
  ) %>% 
  add_trace(
    type = "scatter",
    mode = "markers",
    marker = list(size = 20L)
    # ,
    # colors = rainbow(5)
  ) %>% 
  hide_colorbar() %>%
  hide_guides() %>% 
  layout(
    xaxis = ax, 
    yaxis = ax) #%>% 
  # add_annotations(
  #   x = vertices_2d$x,
  #   y = vertices_2d$y,
  #   text = sprintf("x%d", seq_len(nrow(vertices_2d))),
  #   xref = "x",
  #   yref = "y",
  #   showarrow = FALSE,
  #   xanchor = "left",
  #   yanchor = "top",
  #   xshift = 10L)



vertices_3d <- data.table(x, y, z, weights)
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
    y = -1.5 * 2, 
    z = 0.75 * 2)
  ),
  xaxis = list(showticklabels = FALSE, showline = TRUE, showgrid = TRUE),
  yaxis = list(showticklabels = FALSE, showline = TRUE, showgrid = TRUE),
  zaxis = list(showticklabels = FALSE, showline = TRUE, showgrid = TRUE))

neighborhood_graph_3d <- plotly::plot_ly(
  vertices_3d, 
  x = ~ x, 
  y = ~ y, 
  z = ~ z) %>% 
  add_trace(
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10L, color = "gray")
  )  %>% 
  hide_colorbar() %>% 
  hide_guides() %>% 
  layout(scene = scene)

for (i in seq_along(edges_3d)) {
  
  neighborhood_graph_3d <- neighborhood_graph_3d %>%
    add_trace(
      x = edges_3d[[i]]$x,
      y = edges_3d[[i]]$y,
      z = edges_3d[[i]]$z,
      type = "scatter3d",
      mode = "segments",
      line = list(size = 20L, color = "gray"))
  
}

neighborhood_graph_3d <- neighborhood_graph_3d %>%
  add_trace(
    x = ~ center_3d$x,
    y = ~ center_3d$y,
    z = ~ center_3d$z,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10L, color = "blue"))
