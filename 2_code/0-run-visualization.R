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
