# ------------------------------------------------------------------------------
# DATA GENERATION
# ------------------------------------------------------------------------------

# Purpose: generate synthetic data

# In:  -
# Out: data sets to run sslle on

# GENERATE AND SAVE DATA SETS --------------------------------------------------

data_incomplete_tire <- make_incomplete_tire(n_points = 300L)
data_swiss_roll <- make_swiss_roll(n_points = 1000L)
data_scurve <- make_s_curve(n_points = 1000L)

data_sets <- list(
  incomplete_tire = data_incomplete_tire, 
  swiss_roll = data_swiss_roll,
  scurve = data_scurve)

save_rdata_files(data_sets, folder = "2_code/1_data")
