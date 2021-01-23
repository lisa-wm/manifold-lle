load_rdata_files(data_sets, folder = "2_code/1_data")
data_unlabeled <- lapply(data_sets, function(i) {i[, .(x, y, z)]})

data <- data_unlabeled$swiss_roll

reconstruction_weights <- compute_reconstruction_weights(
  data,
  "knn",
  5L,
  TRUE,
  1e-4
)

reconstruction_err <- 