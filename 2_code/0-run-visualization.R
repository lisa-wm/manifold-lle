# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

# Create plots used for report and presentation

# S-CURVE ----------------------------------------------------------------------

s_curve <- plot_manifold_3d(make_s_curve(n_points = 10000L))

# FIXME Check out how orca can be used with here()

orca(
  s_curve, 
  "4_report/figures/s-curve.pdf",
  height = 400,
  width = 450
)

# EXAMPLE NEIGHBORHOOD GRAPH ---------------------------------------------------

neighborhood_data <- igraph::make_graph(
  edges = c(
    1, 2, 
    1, 3,
    2, 1, 
    2, 3,
    3, 1,
    3, 2,
    4, 1,
    4, 3,
    5, 1,
    5, 4,
    6, 1,
    6, 2,
    7, 5,
    7, 6), 
  n = 7)

pdf(here("4_report/figures", "neighborhood-graph.pdf"), width = 8, height = 8)

set.seed(1)

plot(
  neighborhood_data,
  vertex.color = "gray", 
  vertex.size = 15, 
  vertex.frame.color = "gray", 
  vertex.label.color = "black", 
  vertex.label.cex = 2,
  edge.color = "black")   

ggsave(
  here("4_report/figures", "neighborhood-graph.pdf"), 
  width = 8, 
  height = 8)
dev.off()

# EXAMPLE NEIGHBORHOOD GRAPH 3D ------------------------------------------------

s_curve_connected <- plot_manifold_3d_connected(
  make_s_curve(n_points = 300L),
  k = 5L) 

orca(
  s_curve_connected, 
  "4_report/figures/s-curve-connected.pdf",
  height = 800,
  width = 900
)

# ILLUSTRATION KERNEL PCA ------------------------------------------------------

spirals_data <- mlbench::mlbench.spirals(
  n = 100L, 
  cycles = 1, 
  sd = 0) %>% 
  as.data.table()
spirals_data <- spirals_data[
  classes == 1 & x.1 < 0.4
  ][, classes := seq(0, 1, length.out = length(.I))
    ][, t := seq(0, 1, length.out = length(.I))]
setnames(spirals_data, c("x", "y", "z", "t"))

n_colors <- nrow(spirals_data)

# FIXME Save via orca w/ correct angles

spirals_1d <- plot_manifold_1d(spirals_data[, .(t)], n_colors)
spirals_2d <- plot_manifold_2d(spirals_data[, .(x, y, t)], n_colors)
spirals_3d <- plot_manifold_3d(spirals_data, n_colors)

orca(
  spirals_1d, 
  "4_report/figures/spirals-1d.pdf",
  height = 400,
  width = 400
)
orca(
  spirals_2d, 
  "4_report/figures/spirals-2d.pdf",
  height = 400,
  width = 300
)
orca(
  spirals_3d, 
  "4_report/figures/spirals-3d.pdf",
  height = 400,
  width = 300
)

# SPHERE WITH TANGENT PLANE ----------------------------------------------------

sphere_tangent_plane <- plot_manifold_3d(make_unit_sphere(n_points = 3000)) %>% 
  add_trace(
    z = matrix(rep(1, 100), ncol = 10),
    x = seq(-1, 1, length.out = 10),
    y = seq(-1, 1, length.out = 10),
    type = "surface",
    color = "gray",
    showlegend = FALSE) %>% 
  hide_colorbar() %>% 
  layout(scene = list(
    camera = list(eye = list(
      x = 2, 
      y = -1.25, 
      z = -0.5)
    )))

orca(
  sphere_tangent_plane, 
  "4_report/figures/sphere-tangent.pdf",
  height = 400,
  width = 450
)