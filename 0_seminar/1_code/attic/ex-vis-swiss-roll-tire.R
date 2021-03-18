library(ggplot2)
library(gridExtra)
library(scatterplot3d)
library(plotly)

# SWISS ROLL -------------------------------------------------------------------

# Swiss roll creation (as implemented in sklearn)

set.seed(123)
rng <- runif(0, 1, n = 10000)
t <- 1.5 * pi * (1 + 2 * rng)
x <- t * cos(t)
y <- 21 * runif(0, 1, n = 10000)
z <- t * sin(t)

df_sw <- data.frame(x, y, z, t)

# View from above

p_1 <- ggplot(df_sw, aes(x = x, y = z, col = t)) +
  scale_color_viridis_c() +
  geom_point()

# View "flattened" --> note how simple projection onto plane fails to unroll!

p_2 <- ggplot(df_sw, aes(x = x, y = y, col = t)) +
  scale_color_viridis_c() +
  geom_point()

grid.arrange(p_1, p_2)

# 3D

make_rainbow <- function(data, n_colors) rainbow(n_colors)[cut(data, n_colors)]

scatterplot3d(
  df_sw[, 1:3],
  color = make_rainbow(df_sw[["t"]], 20))

plot_ly(df_sw, x = ~ x, y = ~ y, z = ~ z, color = t) %>% 
  add_markers()

# INCOMPLETE TIRE --------------------------------------------------------------

set.seed(123)
t <- 5 * pi * runif(0, 1, n = 10000) / 3
set.seed(456)
s <- 5 * pi * runif(0, 1, n = 10000) / 3
x <- data.frame(
  x_1 = (3 + cos(s)) * cos(t),
  x_2 = (3 + cos(s)) * sin(t),
  x_3 = sin(s)
)

df_it <- data.frame(x, s, t)

# t cuts tire in vertical slices

scatterplot3d(
  df_it[, 1:3],
  color = make_rainbow(df_it[["t"]], 10))

# s cuts tire in horizontal slices

scatterplot3d(
  df_it[, 1:3],
  color = make_rainbow(df_it[["s"]], 10))

my_palette = rainbow(10)
plot_ly(df_it, x = ~ x_1, y = ~ x_2, z = ~ x_3, color = t) %>% 
  add_markers(colors = my_palette)
