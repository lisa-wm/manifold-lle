# ------------------------------------------------------------------------------
# EVALUATION
# ------------------------------------------------------------------------------

# VISUAL INSPECTION ------------------------------------------------------------

load_rdata_files(data_sets, folder = "2_code/1_data")
load_rdata_files(result_lle, folder = "2_code")

result_lle_original_coordinates <- lapply(
  result_lle,
  function(i) data.table(i$Y, data_sets$i[, .(t, s)]))

lapply(
  result_lle_original_coordinates,
  plot_manifold,
  dim = 2L)

# result_lle_original_coordinates <- lapply(
#   list(swiss_roll = swiss_roll_sslle),
#   function(i) data.table(i$Y, data_sets$i[, .(t)]))
# 
# plot_manifold(
#   data.table(
#     incomplete_tire_sslle$Y, 
#     data_sets$incomplete_tire[, .(t)]), 
#   dim = 2L)
