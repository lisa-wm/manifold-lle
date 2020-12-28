# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

# Create plots used for report and presentation

# S-CURVE ----------------------------------------------------------------------

s_curve <- plot_manifold_3d(make_s_curve(n_points = 10000))

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
  # edge.arrow.size = .5, 
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
