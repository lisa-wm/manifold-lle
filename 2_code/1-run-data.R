# ------------------------------------------------------------------------------
# DATA GENERATION
# ------------------------------------------------------------------------------

# Purpose: generate synthetic data

# In:  -
# Out: data sets to run sslle on

# GENERATE INCOMPLETE TIRE -----------------------------------------------------

data_incomplete_tire <- make_incomplete_tire(n_points = 1000L)
save_rdata_files(data_incomplete_tire, folder = "2_code/1_data")

# GENERATE SWISS ROLL ----------------------------------------------------------

data_swiss_roll <- make_swiss_roll(n_points = 1000L)
save_rdata_files(data_swiss_roll, folder = "2_code/1_data")