# ------------------------------------------------------------------------------
# CREATING EXAMPLE MANIFOLDS
# ------------------------------------------------------------------------------

# Purpose: create various synthetic data sets for testing

# INCOMPLETE TIRE --------------------------------------------------------------

make_incomplete_tire <- function(n_points, seed = 123L) {
  
  # Perform basic input checks
  
  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Compute incomplete tire coordinates as proposed in Yang et al. (2006)
  
  set.seed(seed)
  t <- 5 * pi * runif(0, 1, n = n_points) / 3
  set.seed(seed + 1L)
  s <- 5 * pi * runif(0, 1, n = n_points) / 3
  x_1 <- (3 + cos(s)) * cos(t)
  x_2 <- (3 + cos(s)) * sin(t)
  x_3 <- sin(s)
  
  data.table(x_1, x_2, x_3, t, s)
  
}

# S-CURVE ----------------------------------------------------------------------

make_s_curve <- function(n_points, seed = 123L) {
  
  # Perform basic input checks
  
  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Compute S-curve coordinates as implemented in Python's sklearn
  
  set.seed(seed)
  t <- 3 * pi * (runif(0, 1, n = n_points) - 0.5)
  x_1 <- sin(t)
  set.seed(seed + 1L)
  x_2 <- 2 * runif(0, 1, n = n_points)
  x_3 <- (sign(t) * (cos(t) - 1))
  
  data.table(x_1, x_2, x_3, t)
  
}

# SWISS ROLL -------------------------------------------------------------------

make_swiss_roll <- function(n_points, seed = 123L) {
  
  # Perform basic input checks

  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Compute Swiss roll coordinates as implemented in Python's sklearn
  
  set.seed(seed)
  t <- 1.5 * pi * (1 + 2 * runif(0, 1, n = n_points))
  x_1 <- t * cos(t)
  set.seed(seed + 1L)
  x_2 <- 21 * runif(0, 1, n = n_points)
  x_3 <- t * sin(t)
  s <- x_2
  
  data.table(x_1, x_2, x_3, t, s)
  
}

# UNIT SPHERE ------------------------------------------------------------------

make_unit_sphere <- function(n_points, seed = 123L) {
  
  # Perform basic input checks
  
  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Source: https://github.com/rrrlw/TDAstats/blob/master/data-raw/sphere3d.R
  
  res <- data.table(
    x_1 = rep(0, n_points),
    x_2 = rep(0, n_points),
    x_3 = rep(0, n_points),
    t = rep(0, n_points))
  
  set.seed(seed)
  
  for (i in seq_len(n_points)) {
    
    # Pick appropriate pair of x, y within unit sphere
    
    x <- y <- 1L
    
    while(x^2 + y^2 > 1) {

      x <- runif(1L, -1L, 1L)
      y <- runif(1L, -1L, 1L)
      
    }
    
    # Compute coordinates
    
    res[i, "x_1"] <- 2 * x * sqrt(1 - x^2 - y^2)
    res[i, "x_2"] <- 2 * y * sqrt(1 - x^2 - y^2)
    res[i, "x_3"] <- 1 - 2 * (x^2 + y^2)
    res[i, "t"] <- 1 - 2 * (x^2 + y^2)
    
  }

  res
  
}

# WOLRD DATA -------------------------------------------------------------------

make_world_data_3d <- function(file) {
  
  dt <- data.table::fread(file)
  
  dt[
    , x_3 := -1.5 * x_3 # without scaling, globe looks oblong
    ][, `:=` (t = y, y = NULL)]
  
  dt

}

make_world_data_2d <- function(file) {
  
  dt <- data.table::fread(file)
  
  dt[, `:=` (t = y, y = NULL)]
  
  dt
  
}