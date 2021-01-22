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

make_incomplete_tire <- function(n_points, seed = 123L) {
  
  # Perform basic input checks
  
  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
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

make_swiss_roll <- function(n_points, seed = 123L) {
  
  # Perform basic input checks

  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Compute Swiss roll coordinates as implemented in Python's sklearn
  
  set.seed(seed)
  t <- 1.5 * pi * (1 + 2 * runif(0, 1, n = n_points))
  x <- t * cos(t)
  set.seed(seed + 1L)
  y <- 21 * runif(0, 1, n = n_points)
  z <- t * sin(t)
  
  data.table(x, y, z, t)
  
}

# S-CURVE ----------------------------------------------------------------------

#' Create Swiss roll data
#'
#' @param seed Single numeric value for creating an RNG seed
#' @param n_points Number of points to sample from manifold
#' @return Data table object containing R3 coordinates (columns x, y, z) and
#' main manifold dimension (column t)

make_s_curve <- function(n_points, seed = 123L) {
  
  # Perform basic input checks
  
  checkmate::assert_number(seed)
  checkmate::assert_count(n_points)
  
  # Compute S-curve coordinates as implemented in Python's sklearn
  
  set.seed(seed)
  t <- 3 * pi * (runif(0, 1, n = n_points) - 0.5)
  x <- sin(t)
  set.seed(seed + 1L)
  y <- 2 * runif(0, 1, n = n_points)
  z <- (sign(t) * (cos(t) - 1))
  
  data.table(x, y, z, t)

}

# UNIT SPHERE ------------------------------------------------------------------

#' Create unit sphere embeddedin R3
#'
#' @param seed Single numeric value for creating an RNG seed
#' @param n_points Number of points to sample from manifold
#' @return Data table object containing R3 coordinates (columns x, y, z) and
#' main manifold dimension (column t)

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

# CIRCLE DATA ------------------------------------------------------------------



# WORLD DATA -------------------------------------------------------------------