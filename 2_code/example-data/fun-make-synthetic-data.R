# ------------------------------------------------------------------------------
# CREATING EXAMPLE MANIFOLDS
# ------------------------------------------------------------------------------

# INCOMPLETE TIRE --------------------------------------------------------------

#' Create incomplete tire data
#'
#' @param seed Single numeric value for creating an RNG seed
#' @param n_points Number of points to sample from manifold
#' @return Data table object containing R3 coordinates (columns x, y, z) and
#' main manifold dimensions (columns s, t)

make_incomplete_tire <- function(seed = 123L, n_points) {
  
  # Perform basic input checks
  
  assert_number(seed)
  assert_count(n_points)
  
  # Compute incomplete tire coordinates as proposed in Yang et al. (2006)
  
  set.seed(seed)
  t <- 5 * pi * runif(0, 1, n = n_points) / 3
  set.seed(seed + 1L)
  s <- 5 * pi * runif(0, 1, n = n_points) / 3
  x <- (3 + cos(s)) * cos(t)
  y <- (3 + cos(s)) * sin(t)
  z <- sin(s)
  
  data.table(x, y, z, t, s)
  
}

# SWISS ROLL -------------------------------------------------------------------

#' Create Swiss roll data
#'
#' @param seed Single numeric value for creating an RNG seed
#' @param n_points Number of points to sample from manifold
#' @return Data table object containing R3 coordinates (columns x, y, z) and
#' main manifold dimension (column t)

make_swiss_roll <- function(seed = 123L, n_points) {
  
  # Perform basic input checks

  assert_number(seed)
  assert_count(n_points)
  
  # Compute Swiss roll coordinates as implemented in Python's sklearn
  
  set.seed(seed)
  t <- 1.5 * pi * (1 + 2 * runif(0, 1, n = n_points))
  x <- t * cos(t)
  set.seed(seed + 1L)
  y <- 21 * runif(0, 1, n = n_points)
  z <- t * sin(t)
  
  data.table(x, y, z, t)
  
}

# CIRCLE DATA ------------------------------------------------------------------



# WORLD DATA -------------------------------------------------------------------