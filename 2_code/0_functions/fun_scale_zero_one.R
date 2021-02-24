# ------------------------------------------------------------------------------
# SCALE VECTOR TO ZERO-ONE RANGE
# ------------------------------------------------------------------------------

# Purpose: scale vector to zero-one range for harmonization

scale_zero_one <- function(x) {
  
  (x - min(x)) / (max(x) - min(x))
  
}
