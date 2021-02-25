# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

# Create plots used for report and presentation

# S-CURVE ----------------------------------------------------------------------

s_curve_10k <- plot_manifold(make_s_curve(n_points = 1000L), dim = 3L)

orca(
  s_curve_10k, 
  "4_report/figures/s-curve.pdf",
  height = 400,
  width = 450)

# S-CURVE UNDONE ---------------------------------------------------------------

s_curve_data <- make_s_curve(n_points = 1000L)
s_curve <- plot_manifold(s_curve_data, dim = 3L)
s_curve_undone <- plot_manifold(s_curve_data[, .(x_1, x_2, t, t)], dim = 2L)

orca(
  s_curve, 
  "3_presentation/figures/s-curve.pdf",
  height = 400,
  width = 450)

orca(
  s_curve_undone, 
  "3_presentation/figures/s-curve-undone.pdf",
  height = 400,
  width = 1000)

# S-CURVE WITH PRIOR POINTS ----------------------------------------------------

n_landmarks <- 12L

prior_points_poor <- data.table::copy(s_curve_data)
data.table::setorder(prior_points_poor, s)
prior_points_poor <- prior_points_poor[seq_len(n_landmarks)]

prior_points_random <- find_landmarks(
  s_curve_data,
  n_landmarks = n_landmarks,
  method = "random")

prior_points_maxmin <- find_landmarks(
  s_curve_data,
  n_landmarks = n_landmarks,
  n_neighbors = 15L,
  method = "maxmin")

s_curve_pp_poor <- s_curve %>% 
  add_trace(
    x = ~ prior_points_poor$x_1,
    y = ~ prior_points_poor$x_2,
    z = ~ prior_points_poor$x_3,
    color = ~ 1L,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 10L)
  ) %>% 
  hide_guides()

s_curve_pp_random <- s_curve %>% 
  add_trace(
    x = ~ s_curve_data[prior_points_random]$x_1,
    y = ~ s_curve_data[prior_points_random]$x_2,
    z = ~ s_curve_data[prior_points_random]$x_3,
    color = ~ 1L,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 10L)
  ) %>% 
  hide_guides()

s_curve_pp_maxmin <- s_curve %>% 
  add_trace(
    x = ~ s_curve_data[prior_points_maxmin]$x_1,
    y = ~ s_curve_data[prior_points_maxmin]$x_2,
    z = ~ s_curve_data[prior_points_maxmin]$x_3,
    color = ~ 1L,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 10L)
  ) %>% 
  hide_guides()

s_curve_undone_pp_random <- s_curve_undone %>%
  add_trace(
    x = ~ s_curve_data[prior_points_random]$x_1,
    y = ~ s_curve_data[prior_points_random]$x_2,
    color = ~ 1L,
    type = "scatter",
    mode = "markers",
    marker = list(color = "black", size = 20L)
  ) %>%
  hide_guides()

# s_curve_undone_pp_maxmin <- s_curve_undone %>% 
#   add_trace(
#     x = ~ s_curve_data[prior_points_maxmin]$x_1,
#     y = ~ s_curve_data[prior_points_maxmin]$x_2,
#     color = ~ 1L,
#     type = "scatter",
#     mode = "markers",
#     marker = list(color = "black", size = 20L)
#   ) %>% 
#   hide_guides()
# 
# s_curve_undone_pp_poor <- s_curve_undone %>% 
#   add_trace(
#     x = ~ prior_points_poor$x_1,
#     y = ~ prior_points_poor$x_2,
#     color = ~ 1L,
#     type = "scatter",
#     mode = "markers",
#     marker = list(color = "black", size = 20L)
#   ) %>% 
#   hide_guides()


orca(
  s_curve_pp_poor, 
  "3_presentation/figures/s-curve-pp-poor.pdf",
  height = 400,
  width = 450)

orca(
  s_curve_pp_random, 
  "3_presentation/figures/s-curve-pp-random.pdf",
  height = 400,
  width = 450)

orca(
  s_curve_pp_maxmin, 
  "3_presentation/figures/s-curve-pp-maxmin.pdf",
  height = 400,
  width = 450)

orca(
  s_curve_undone_pp_random, 
  "3_presentation/figures/s-curve-pp-undone-random.pdf",
  height = 400,
  width = 1000)

# SWISS ROLL -------------------------------------------------------------------

swiss_roll <- plot_manifold(
  make_swiss_roll(n_points = 1000L), 
  dim = 3L, 
  camera_eye = list(
    x = 0.75, 
    y = -1.5, 
    z = 0.25))

orca(
  swiss_roll, 
  "3_presentation/figures/swiss-roll.pdf",
  height = 400,
  width = 450)

# INCOMPLETE TIRE --------------------------------------------------------------

incomplete_tire <- plot_manifold(
  make_incomplete_tire(n_points = 1000L), 
  dim = 3L, 
  camera_eye = list(
    x = 1.25, 
    y = 0, 
    z = 0.75))

orca(
  incomplete_tire, 
  "3_presentation/figures/incomplete-tire.pdf",
  height = 400,
  width = 600)

# EXAMPLE NEIGHBORHOOD GRAPH 3D ------------------------------------------------

s_curve_connected <- plot_manifold_3d_connected(
  make_s_curve(n_points = 300L),
  k = 3L) 

orca(
  s_curve_connected, 
  "4_report/figures/s-curve-connected.pdf",
  height = 800,
  width = 900)

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

data.table::setnames(spirals_data, c("x", "y", "z", "t"))

n_colors <- nrow(spirals_data)

# FIXME Save via orca w/ correct angles

spirals_1d <- plot_manifold(
  spirals_data[, .(x, t, t)], 
  dim = 1L, 
  n_colors = n_colors)

spirals_2d <- plot_manifold(
  spirals_data[, .(x, y, t, t)], 
  dim = 2L, 
  n_colors = n_colors)

spirals_3d <- plot_manifold(
  spirals_data[, .(x, y, z, t, t)], 
  dim = 3L, 
  n_colors = n_colors,
  camera_eye = list(
    x = 1, 
    y = -1.2, 
    z = 0.5))

orca(
  spirals_1d, 
  "4_report/figures/spirals-1d.pdf",
  height = 50,
  width = 400)

orca(
  spirals_2d, 
  "4_report/figures/spirals-2d.pdf",
  height = 300,
  width = 300)

orca(
  spirals_3d, 
  "4_report/figures/spirals-3d.pdf",
  height = 800,
  width = 1200)

# SPHERE WITH TANGENT PLANE ----------------------------------------------------

sphere_tangent_plane <- plot_manifold(
  make_unit_sphere(n_points = 3000), 
  dim = 3L) %>% 
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
      z = -0.5))))

orca(
  sphere_tangent_plane, 
  "4_report/figures/sphere-tangent.pdf",
  height = 400,
  width = 450)

# EXAMPLE RECONSTRUCTION -------------------------------------------------------

# Coordinates

x <- c(1.2, 1.5, 2, 1, 1.8)
y <- c(1.4, 2, 1.8, 1.6, 1.4)
z <- c(0.5, 1, 1, 0.8, 0.5)
weights <- c(0.2, 0.1, 0.2, 0.3, 0.2)

# 2D plot

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
    type = "scatter",
    mode = "markers",
    marker = list(size = 20L, color = "gray")) %>% 
  hide_colorbar()

label_positions_2d <- data.table(
  x = sapply(seq_along(edges_2d), function(i) mean(edges_2d[[i]]$x)),
  y = sapply(seq_along(edges_2d), function(i) mean(edges_2d[[i]]$y)))

neighborhood_graph_2d <- neighborhood_graph_2d %>% 
  add_annotations(
    x = ~ label_positions_2d$x,
    y = ~ label_positions_2d$y,
    text = sprintf("w<sub>i,%d</sub>", seq_along(edges_2d)),
    xref = "x",
    yref = "y",
    bgcolor = "white",
    font = list(size = 20),
    showarrow = FALSE)

# 3D plot

vertices_3d <- data.table(x, y, z, weights)

center_3d <- data.table(
  x = sum(vertices_3d$x * vertices_3d$weights),
  y = sum(vertices_3d$y * vertices_3d$weights),
  z = sum(vertices_3d$z * vertices_3d$weights))

edges_3d <- lapply(
  seq_len(nrow(vertices_3d)), 
  function(i) {rbind(vertices_3d[i, .(x, y, z)], center_3d)})

ax <- list(
  showticklabels = FALSE, 
  showline = TRUE, 
  showgrid = FALSE, 
  title = "")

scene <- list(
  camera = list(eye = list(
    x = 1.6, 
    y = -1.8, 
    z = 0.6)),
  xaxis = ax,
  yaxis = ax,
  zaxis = ax)

neighborhood_graph_3d <- plotly::plotly_empty() %>% 
  layout(scene = scene, uniformtext = list(minsize = 30))

for (i in seq_len(nrow(vertices_3d))) {
  
  neighborhood_graph_3d <- neighborhood_graph_3d %>%
    add_trace(
      x = edges_3d[[i]]$x,
      y = edges_3d[[i]]$y,
      z = edges_3d[[i]]$z,
      type = "scatter3d",
      mode = "segments",
      line = list(size = 10L, color = "gray"))
  
}

for (i in seq_along(edges_3d)) {
  
  neighborhood_graph_3d <- neighborhood_graph_3d %>%
    add_trace(
      x = edges_3d[[i]]$x,
      y = edges_3d[[i]]$y,
      z = edges_3d[[i]]$z,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 10L, color = rainbow(nrow(vertices_3d))[i])) %>% 
    hide_guides()
  
}

neighborhood_graph_3d <- neighborhood_graph_3d %>%
  add_trace(
    x = ~ center_3d$x,
    y = ~ center_3d$y,
    z = ~ center_3d$z,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10L, color = "gray")) %>% 
  hide_colorbar()

label_positions_3d <- data.table(
  x = sapply(seq_along(edges_3d), function(i) mean(edges_3d[[i]]$x)),
  y = sapply(seq_along(edges_3d), function(i) mean(edges_3d[[i]]$y)),
  z = sapply(seq_along(edges_3d), function(i) mean(edges_3d[[i]]$z)))

neighborhood_graph_3d <- neighborhood_graph_3d %>% 
  add_trace(
    type = "scatter3d",
    mode = "text",
    x = ~ label_positions_3d$x,
    y = ~ label_positions_3d$y,
    z = ~ label_positions_3d$z,
    text = sprintf("w<sub>i,%d</sub>", seq_along(edges_3d)),
    showlegend = FALSE,
    inherit = FALSE)

orca(
  neighborhood_graph_2d,
  "4_report/figures/reconstruction-2d.pdf",
  height = 300,
  width = 300)

orca(
  neighborhood_graph_3d, 
  "4_report/figures/reconstruction-3d.pdf",
  height = 300,
  width = 300)

# SENSITIVITY ANALYSIS ---------------------------------------------------------

load_rdata_files(sensitivity_landmarks_plots_quant, folder = "2_code")
load_rdata_files(sensitivity_noise_plots_quant, folder = "2_code")
load_rdata_files(sensitivity_landmarks_plots_qual, folder = "2_code")
load_rdata_files(sensitivity_noise_plots_qual, folder = "2_code")
load_rdata_files(sensitivity_landmarks_plots_key_variation, folder = "2_code")
load_rdata_files(sensitivity_noise_plots_key_variation, folder = "2_code")

pdf(
  here("3_presentation/figures", "sensitivity_landmarks_auc.pdf"),
  width = 16, 
  height = 6)

gridExtra::grid.arrange(
  sensitivity_landmarks_plots_quant$swiss_roll$auc_plot,
  sensitivity_landmarks_plots_quant$incomplete_tire$auc_plot, 
  ncol = 2L)

ggplot2::ggsave(
  here("3_presentation/figures", "sensitivity_landmarks_auc.pdf"),
  width = 16, 
  height = 6)
dev.off()

orca(
  sensitivity_landmarks_plots_qual$swiss_roll, 
  "3_presentation/figures/sensitivity_landmarks_qual_swiss.pdf",
  height = 450,
  width = 1000)

orca(
  sensitivity_landmarks_plots_qual$incomplete_tire, 
  "3_presentation/figures/sensitivity_landmarks_qual_tire.pdf",
  height = 450,
  width = 1000)

orca(
  sensitivity_landmarks_plots_key_variation$swiss_roll, 
  "3_presentation/figures/sensitivity_landmarks_key_swiss.pdf",
  height = 450,
  width = 1000)

orca(
  sensitivity_landmarks_plots_key_variation$incomplete_tire, 
  "3_presentation/figures/sensitivity_landmarks_key_tire.pdf",
  height = 450,
  width = 1000)

pdf(
  here("3_presentation/figures", "sensitivity_noise_auc.pdf"),
  width = 16, 
  height = 6)

gridExtra::grid.arrange(
  sensitivity_noise_plots_quant$swiss_roll$auc_plot,
  sensitivity_noise_plots_quant$incomplete_tire$auc_plot, 
  ncol = 2L)

ggplot2::ggsave(
  here("3_presentation/figures", "sensitivity_noise_auc.pdf"),
  width = 16, 
  height = 6)
dev.off()

orca(
  sensitivity_noise_plots_qual$swiss_roll, 
  "3_presentation/figures/sensitivity_noise_qual_swiss.pdf",
  height = 500,
  width = 1000)

orca(
  sensitivity_noise_plots_qual$incomplete_tire, 
  "3_presentation/figures/sensitivity_noise_qual_tire.pdf",
  height = 500,
  width = 1000)

orca(
  sensitivity_noise_plots_key_variation$swiss_roll, 
  "3_presentation/figures/sensitivity_noise_key_swiss.pdf",
  height = 450,
  width = 1000)

orca(
  sensitivity_noise_plots_key_variation$incomplete_tire, 
  "3_presentation/figures/sensitivity_noise_key_tire.pdf",
  height = 450,
  width = 1000)