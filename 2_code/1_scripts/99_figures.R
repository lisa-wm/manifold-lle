# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

# Create plots used for report and presentation

# S-CURVE ----------------------------------------------------------------------

plotly::orca(
  plot_manifold(
    data = make_s_curve(n_points = 1000L)[, .(x_1, x_2, x_3)],
    intrinsic_coords = make_s_curve(n_points = 1000L)[, .(t)]), 
  "4_report/figures/s_curve.pdf",
  height = 400,
  width = 450)

# S-CURVE UNDONE ---------------------------------------------------------------

plotly::orca(
  plot_manifold(
    data = make_s_curve(n_points = 1000L)[, .(x_1, x_2, x_3)],
    intrinsic_coords = make_s_curve(n_points = 1000L)[, .(t)]), 
  "3_presentation/figures/s_curve.pdf",
  height = 400,
  width = 450)

plotly::orca(
  plot_manifold(
    data = make_s_curve(n_points = 1000L)[, .(t, x_2)],
    intrinsic_coords = make_s_curve(n_points = 1000L)[, .(t)]), 
  "3_presentation/figures/s_curve_undone.pdf",
  height = 400,
  width = 1000)

# S-CURVE WITH PRIOR POINTS ----------------------------------------------------

s_curve_data <- make_s_curve(n_points = 1000L)
s_curve_plot <- plot_manifold(
  data = s_curve_data[, .(x_1, x_2, x_3)],
  intrinsic_coords = s_curve_data[, .(t)])
n_landmarks <- 12L

prior_points_poor <- data.table::copy(s_curve_data)
data.table::setorder(prior_points_poor, t)
prior_points_poor <- order(s_curve_data$t)[1:n_landmarks]

prior_points_random <- find_landmarks(
  s_curve_data[, .(x_1, x_2, x_3)],
  n_landmarks = n_landmarks,
  method = "random")

prior_points_maxmin <- find_landmarks(
  s_curve_data,
  n_landmarks = n_landmarks,
  n_neighbors = 15L,
  method = "maxmin")

plots_s_curve <- lapply(
  
  list(
    s_curve_pp_poor = prior_points_poor,
    s_curve_pp_random = prior_points_random,
    s_curve_pp_maxmin = prior_points_maxmin),
  
  function(i) {
    
    s_curve_plot %>% 
      add_trace(
        x = ~ s_curve_data[i]$x_1,
        y = ~ s_curve_data[i]$x_2,
        z = ~ s_curve_data[i]$x_3,
        color = ~ 1L,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = "black", size = 10L)
      ) %>% 
      hide_guides()
    
  }
)

invisible(lapply(
  names(plots_s_curve),
  function(i) {
    plotly::orca(
      get(i), 
      paste0("3_presentation/figures/", i, ".pdf"),
      height = 400,
      width = 450)
  }))

plotly::orca(
  
)

s_curve_undone_pp_random <- plot_manifold(
  data = s_curve_data[, .(t, x_2)],
  intrinsic_coords = s_curve_data[, .(t)]) %>%
  add_trace(
    x = ~ s_curve_data[prior_points_random]$t,
    y = ~ s_curve_data[prior_points_random]$x_2,
    color = ~ 1L,
    type = "scatter",
    mode = "markers",
    marker = list(color = "black", size = 20L)
  ) %>%
  hide_guides()

plotly::orca(
  plot_manifold(
    data = s_curve_data[, .(t, x_2)],
    intrinsic_coords = s_curve_data[, .(t)]) %>%
    add_trace(
      x = ~ s_curve_data[prior_points_random]$t,
      y = ~ s_curve_data[prior_points_random]$x_2,
      color = ~ 1L,
      type = "scatter",
      mode = "markers",
      marker = list(color = "black", size = 20L)
    ) %>%
    hide_guides(), 
  "3_presentation/figures/s_curve_undone_pp_random.pdf",
  height = 400,
  width = 1000)

# SWISS ROLL -------------------------------------------------------------------

plotly::orca(
  plot_manifold(
    data = make_swiss_roll(n_points = 1000L)[, .(x_1, x_2, x_3)], 
    intrinsic_coords = make_swiss_roll(n_points = 1000L)[, .(t)], 
    camera_eye = list(
      x = 0.75, 
      y = -1.5, 
      z = 0.25)), 
  "3_presentation/figures/swiss_roll.pdf",
  height = 400,
  width = 450)

# INCOMPLETE TIRE --------------------------------------------------------------

plotly::orca(
  plot_manifold(
    data = make_incomplete_tire(n_points = 1000L)[, .(x_1, x_2, x_3)], 
    intrinsic_coords = make_incomplete_tire(n_points = 1000L)[, .(t)], 
    camera_eye = list(
      x = 1.25, 
      y = 0, 
      z = 0.75)), 
  "3_presentation/figures/incomplete_tire.pdf",
  height = 400,
  width = 600)

# EXAMPLE NEIGHBORHOOD GRAPH 3D ------------------------------------------------

plotly::orca(
  plot_manifold_3d_connected(
    data = make_s_curve(n_points = 300L)[, .(x_1, x_2, x_3)],
    intrinsic_coords = make_s_curve(n_points = 300L)[, .(t)],
    k = 3L), 
  "4_report/figures/s_curve_connected.pdf",
  height = 800,
  width = 900)

# ILLUSTRATION KERNEL PCA ------------------------------------------------------

spirals_data <- data.table::as.data.table(
  mlbench::mlbench.spirals(
    n = 100L, 
    cycles = 1L, 
    sd = 0L))

spirals_data <- spirals_data[
  classes == 1L & x.1 < 0.4
  ][, classes := seq(0L, 1L, length.out = length(.I))
    ][, t := seq(0L, 1L, length.out = length(.I))]

data.table::setnames(spirals_data, c("x", "y", "z", "t"))

plotly::orca(
  plot_manifold(
    data = spirals_data[, .(t)],
    intrinsic_coords = spirals_data[, .(t)],
    n_colors = nrow(spirals_data)), 
  "4_report/figures/spirals_1d.pdf",
  height = 50,
  width = 400)

plotly::orca(
  plot_manifold(
    data = spirals_data[, .(x, y)],
    intrinsic_coords = spirals_data[, .(t)],
    n_colors = nrow(spirals_data)), 
  "4_report/figures/spirals_2d.pdf",
  height = 300,
  width = 300)

plotly::orca(
  plot_manifold(
    data = spirals_data[, .(x, y, z)],
    intrinsic_coords = spirals_data[, .(t)],
    n_colors = n_colors,
    camera_eye = list(
      x = 1, 
      y = -1.2, 
      z = 0.5)), 
  "4_report/figures/spirals_3d.pdf",
  height = 800,
  width = 1200)

# SPHERE WITH TANGENT PLANE ----------------------------------------------------

sphere_data <- make_unit_sphere(n_points = 3000L)

plotly::orca(
  plot_manifold(
    data = sphere_data[, .(x_1, x_2, x_3)], 
    intrinsic_coords = sphere_data[, .(t)]) %>% 
    add_trace(
      z = matrix(rep(1L, 100L), ncol = 10L),
      x = seq(-1L, 1L, length.out = 10L),
      y = seq(-1L, 1L, length.out = 10L),
      type = "surface",
      color = "gray",
      showlegend = FALSE) %>% 
    hide_colorbar() %>% 
    layout(scene = list(
      camera = list(eye = list(
        x = 2, 
        y = -1.25, 
        z = -0.5)))), 
  "4_report/figures/sphere_tangent.pdf",
  height = 400,
  width = 450)

# EXAMPLE RECONSTRUCTION -------------------------------------------------------

# Coordinates

x <- c(1.2, 1.5, 2, 1, 1.8)
y <- c(1.4, 2, 1.8, 1.6, 1.4)
z <- c(0.5, 1, 1, 0.8, 0.5)
weights <- c(0.2, 0.1, 0.2, 0.3, 0.2)

# Data

vertices_2d <- data.table::data.table(x, y, weights)

center_2d <- data.table::data.table(
  x = sum(vertices_2d$x * vertices_2d$weights),
  y = sum(vertices_2d$y * vertices_2d$weights))

edges_2d <- lapply(
  seq_len(nrow(vertices_2d)), 
  function(i) {rbind(vertices_2d[i, .(x, y)], center_2d)})

vertices_3d <- data.table::data.table(x, y, z, weights)

center_3d <- data.table::data.table(
  x = sum(vertices_3d$x * vertices_3d$weights),
  y = sum(vertices_3d$y * vertices_3d$weights),
  z = sum(vertices_3d$z * vertices_3d$weights))

edges_3d <- lapply(
  seq_len(nrow(vertices_3d)), 
  function(i) {rbind(vertices_3d[i, .(x, y, z)], center_3d)})

# ------------------------------------------------------------------------------

ax <- list(
  showticklabels = FALSE, 
  showline = TRUE, 
  title = "", 
  showgrid = FALSE)

label_positions_2d <- data.table::data.table(
  x = sapply(edges_2d, function(i) mean(i$x)),
  y = sapply(edges_2d, function(i) mean(i$y)))

neighborhood_graph_2d <- plotly::plot_ly(
  do.call(rbind, edges_2d),
  x = ~ x,
  y = ~ y,
  color = ~ 1L) %>%
  add_trace(
    type = "scatter",
    mode = "segments",
    line = list(size = 10L, color = "gray")) %>% 
  add_trace(
    color = ~ seq_len(nrow(do.call(rbind, edges_2d))),
    type = "scatter",
    mode = "markers",
    marker = list(size = 20L, color = rainbow(nrow(do.call(rbind, edges_2d))))
  ) %>% 
  add_trace(
    x = ~ center_2d$x,
    y = ~ center_2d$y,
    type = "scatter",
    mode = "markers",
    marker = list(size = 20L, color = "gray")) %>% 
  add_annotations(
    x = ~ label_positions_2d$x,
    y = ~ label_positions_2d$y,
    text = sprintf("w<sub>i,%d</sub>", seq_along(edges_2d)),
    xref = "x",
    yref = "y",
    bgcolor = "white",
    font = list(size = 20L),
    showarrow = FALSE) %>% 
  hide_guides() %>% 
  layout(xaxis = ax, yaxis = ax)

plotly::orca(
  neighborhood_graph_2d,
  "4_report/figures/reconstruction_2d.pdf",
  height = 300,
  width = 300)

# ------------------------------------------------------------------------------

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

label_positions_3d <- data.table::data.table(
  x = sapply(edges_3d, function(i) mean(i$x)),
  y = sapply(edges_3d, function(i) mean(i$y)),
  z = sapply(edges_3d, function(i) mean(i$z)))

neighborhood_graph_3d <- plotly::plot_ly(
  do.call(rbind, edges_3d),
  x = ~ x,
  y = ~ y,
  z = ~ z,
  color = ~ 1L) %>%
  add_trace(
    type = "scatter3d",
    mode = "segments",
    line = list(size = 10L, color = "gray")) %>% 
  add_trace(
    color = ~ seq_len(nrow(do.call(rbind, edges_3d))),
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10L, color = rainbow(nrow(do.call(rbind, edges_3d))))
  ) %>% 
  add_trace(
    x = ~ center_3d$x,
    y = ~ center_3d$y,
    z = ~ center_3d$z,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10L, color = "gray")) %>% 
  add_trace(
    type = "scatter3d",
    mode = "text",
    x = ~ label_positions_3d$x,
    y = ~ label_positions_3d$y,
    z = ~ label_positions_3d$z,
    text = sprintf("w<sub>i,%d</sub>", seq_along(edges_3d)),
    showlegend = FALSE,
    inherit = FALSE) %>% 
  hide_guides() %>% 
  layout(scene = scene, uniformtext = list(minsize = 30)) 

plotly::orca(
  neighborhood_graph_3d, 
  "4_report/figures/reconstruction_3d.pdf",
  height = 300,
  width = 300)

# RNX CURVE --------------------------------------------------------------------

load_rdata_files(data_labeled, folder = "2_code/2_data")
data_unlabeled <- lapply(data_labeled, function(i) {i[, .(x_1, x_2, x_3)]})

embeddings <- lapply(data_unlabeled, function(i) dimRed::embed(i, "PCA"))
names(embeddings) <- c("incomplete tire", "swiss roll")

rnx_curve_dimred <- dimRed::plot_R_NX(embeddings)$data

rnx_curve_ <- ggplot2::ggplot(
  rnx_curve_dimred, 
  aes(x = K, y = R_NX, col = embedding)) +
  geom_line(size = 1.5) + 
  ggplot2::scale_x_log10(
    labels = scales::trans_format(
      "log10",
      scales::math_format()), expand = c(0, 0)) + 
  ggplot2::scale_y_continuous(
    expression(R[NX](k)), 
    limits = c(0, 1), 
    expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(size = 25L)) +
  scale_color_manual(
    values = c("darkgray", "darkslategray4"),
    labels = sprintf(
      "%s: \nAUC = %.2f",
      names(embeddings),
      sapply(embeddings, dimRed::AUC_lnK_R_NX)),
    name = "") +
  xlab("k") +
  ggtitle("Exemplary embedding with PCA")

ggplot2::ggsave(
  here("3_presentation/figures", "rnx_curve.pdf"),
  width = 8L, 
  height = 4L)

# SENSITIVITY ANALYSIS ---------------------------------------------------------

load_rdata_files(sensitivity_plots_auc, "2_code/2_data")
load_rdata_files(sensitivity_plots_emb, "2_code/2_data")
load_rdata_files(sensitivity_plots_key, "2_code/2_data")

# ------------------------------------------------------------------------------

ggplot2::ggsave(
  here("3_presentation/figures", "sensitivity_landmarks_auc.pdf"),
  gridExtra::grid.arrange(
    sensitivity_plots_auc$swiss_roll$auc_plot_landmarks,
    sensitivity_plots_auc$incomplete_tire$auc_plot_landmarks, 
    ncol = 2L),
  width = 16, 
  height = 6)

ggplot2::ggsave(
  here("3_presentation/figures", "sensitivity_noise_auc.pdf"),
  gridExtra::grid.arrange(
    sensitivity_plots_auc$swiss_roll$auc_plot_noise,
    sensitivity_plots_auc$incomplete_tire$auc_plot_noise, 
    ncol = 2L),
  width = 16, 
  height = 6)

# ------------------------------------------------------------------------------

height_qual_landmarks <- 450L
height_key_landmarks <- 220L
height_qual_noise <- 520L
height_key_noise <- 200L
width_all <- 1000L

invisible(lapply(
  
  c("swiss_roll", "incomplete_tire"),
  
  function(i) {
    
    plotly::orca(
      sensitivity_plots_emb[[i]]$emb_plots_landmarks, 
      sprintf("3_presentation/figures/sensitivity_landmarks_qual_%s.pdf", i),
      height = height_qual_landmarks,
      width = width_all)
    
    plotly::orca(
      sensitivity_plots_key[[i]]$plot_coverage, 
      sprintf("3_presentation/figures/sensitivity_landmarks_key_%s.pdf", i),
      height = height_key_landmarks,
      width = width_all)
    
    plotly::orca(
      sensitivity_plots_emb[[i]]$emb_plots_noise, 
      sprintf("3_presentation/figures/sensitivity_noise_qual_%s.pdf", i),
      height = height_qual_noise,
      width = width_all)
    
    plotly::orca(
      sensitivity_plots_key[[i]]$plot_noise, 
      sprintf("3_presentation/figures/sensitivity_noise_key_%s.pdf", i),
      height = height_key_noise,
      width = width_all)
   
  }))

# COMPARISON - LLE & HLLE ------------------------------------------------------

load_rdata_files(comp_lle, folder = "2_code/2_data")

invisible(lapply(
  
  c("swiss_roll", "incomplete_tire"),
  
  function(i) {
    plotly::orca(
      comp_lle[[i]], 
      sprintf("3_presentation/figures/comparison_%s.pdf", i),
      height = 200L,
      width = 100L)
  }))

# COMPARISON - WORLD DATA ------------------------------------------------------

plotly::orca(
  plot_manifold(
    data = make_world_data_3d(
      here("2_code/2_data", "rawdata_world_3d.csv"))[, .(x_1, x_2, x_3)],
    intrinsic_coords = make_world_data_3d(
      here("2_code/2_data", "rawdata_world_3d.csv"))[, .(t = t)],
    camera_eye = list(
      x = 0.4, 
      y = -1.8, 
      z = -0.1), 
    point_size_3d = 3L),
  "3_presentation/figures/world_3d.pdf",
  height = 800L,
  width = 800L)

plotly::orca(
  plot_manifold(
    data = make_world_data_2d(
      here("2_code/2_data", "rawdata_world_2d.csv"))[, .(x_1, x_2)],
    intrinsic_coords = make_world_data_2d(
      here("2_code/2_data", "rawdata_world_2d.csv"))[, .(t = t)],
    point_size = 5L),
  "3_presentation/figures/world_2d.pdf",
  height = 350L,
  width = 800L)

load_rdata_files(comp_lle, folder = "2_code/2_data")

plotly::orca(
  comp_lle$world_data, 
  sprintf("3_presentation/figures/comparison_world.pdf", i),
  height = 200L,
  width = 100L)
