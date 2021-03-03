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
  s <- 3 * pi * (runif(0, 1, n = n_points) - 0.5)
  x_1 <- sin(s)
  set.seed(seed + 1L)
  x_2 <- 2 * runif(0, 1, n = n_points)
  x_3 <- (sign(s) * (cos(s) - 1))
  t <- x_1
  
  data.table(x_1, x_2, x_3, s, t)
  
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
    x = rep(0, n_points),
    y = rep(0, n_points),
    z = rep(0, n_points),
    t = rep(0, n_points)
  )
  
  for (i in seq_len(n_points)) {
    
    # Pick appropriate pair of x, y within unit sphere
    
    x <- y <- 1L
    
    while(x^2 + y^2 > 1) {

      x <- runif(1L, -1L, 1L)
      y <- runif(1L, -1L, 1L)
      
    }
    
    # Compute coordinates
    
    res[i, "x"] <- 2 * x * sqrt(1 - x^2 - y^2)
    res[i, "y"] <- 2 * y * sqrt(1 - x^2 - y^2)
    res[i, "z"] <- 1 - 2 * (x^2 + y^2)
    res[i, "t"] <- 1 - 2 * (x^2 + y^2)
    
  }

  res
  
}

# WOLRD DATA -------------------------------------------------------------------

make_world_data_3d <- function(file) {
  
  dt <- data.table::fread(file)
  
  x_1 <- dt$x_1
  x_2 <- dt$x_2
  x_3 <- -1.5 * dt$x_3
  t <- s <- dt$y
  
  # FIXME find proper scale for third dimension
  
  data.table(x_1, x_2, x_3, t, s)
  
}

make_world_data_2d <- function(file) {
  
  dt <- data.table::fread(file)
  
  x_1 <- scale_zero_one(dt$x_1)
  x_2 <- scale_zero_one(dt$x_2)
  t <- s <- dt$y
  
  data.table(x_1, x_2, t, s)
  
}

world_data_3d <- make_world_data_3d(
  here("2_code/2_data", "rawdata_world_3d.csv"))
plot_manifold(world_data_3d, n_colors = length(unique(world_data$t)), dim = 3L)

world_data_2d <- make_world_data_2d(
  here("2_code/2_data", "rawdata_world_2d.csv"))
plot_manifold(world_data_2d, n_colors = length(unique(world_data$t)), dim = 2L)