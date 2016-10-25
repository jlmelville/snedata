# simulation data used in "How to Use t-SNE Effectively"
# http://distill.pub/2016/misread-tsne/
# https://github.com/distillpub/post--misread-tsne

# map a vector x to a new linear scale in the range (from, to)
linear_map <- function(x, from, to) {
  (x - min(x)) / max(x - min(x)) * (to - from) + from
}

# For 2D data only: linearly map the x and y columns to values in the range
# (0, 255) and set the green and blue channels to those values respectively.
# The red channel is fixed at 20.
add_spatial_colors <- function(points) {
  c1 <- floor(linear_map(points$x, 0, 255))
  c2 <- floor(linear_map(points$y, 0, 255))
  points$color <- rgb(20, c1, c2, maxColorValue = 255, alpha = 255)
  points
}

# Data in the shape of a 2D grid
grid_data <- function(size) {
  points <- expand.grid(1:size, 1:size)
  colnames(points) <- c("x", "y")
  add_spatial_colors(points)
}

# Gaussian cloud, symmetric, of given dimension.
gaussian_data <- function(n, dim, sd = 1, color = NULL) {
  df <- data.frame(matrix(rnorm(n * dim, sd = sd), ncol = dim))
  if (!is.null(color)) {
    df$color <- color
  }
  df
}

# Elongated Gaussian ellipsoid.
long_gaussian_data <- function(n, dim) {
  df <- gaussian_data(n, dim)
  for (j in 1:dim) {
    df[, j] <- df[, j] / j
  }
  df
}

# Return a color for the given angle (in radians)
angle_color <- function(r) {
  hue <- floor(300 * r / (2 * pi))
  hsl_to_rgb(hue, 50, 50)
}

angle_colors <- function(x) {
  vapply(x, angle_color, "")
}

# Helper function needed by hsl_to_rgb, from the CSS spec
# https://www.w3.org/TR/2011/REC-css3-color-20110607/#hsl-color
hue_to_rgb <- function(m1, m2, h) {
  if (h < 0) h <- h + 1
  if (h > 1) h <- h - 1
  if (h * 6 < 1) return(m1 + (m2 - m1) * h * 6)
  if (h * 2 < 1) return(m2)
  if (h * 3 < 2) return(m1 + (m2 - m1) * (2 / 3 - h) * 6)
  m1
}

# Converts HSL to RGB, using the algorithm specified by the CSS spec
# https://www.w3.org/TR/2011/REC-css3-color-20110607/#hsl-color
# h is the hue in degrees in the range (0, 360] (i.e. 360 is excluded).
# s is the saturation as % in the range (0, 100)
# l is the luminance as % in the range (0, 100).
# Returns an RGB string.
hsl_to_rgb <- function(h, s, l) {
  # Map degrees to range (0, 360)
  h <- (((h %% 360) + 360) %% 360) / 360
  s <- s / 100
  l <- l / 100

  if (s == 0) {
    # achromatic
    r <- g <- b <- l
  }
  else {
    if (l <= 0.5) {
      m2 <- l * (s + 1)
    }
    else {
      m2 <- l + s - l * s
    }
    m1 <- l * 2 - m2
    r <- hue_to_rgb(m1, m2, h + 1 / 3)
    g <- hue_to_rgb(m1, m2, h)
    b <- hue_to_rgb(m1, m2, h - 1 / 3)
  }
  rgb(red = r, green = g, blue = b)
}

uniform_angle <- function(num_points) {
  (0:(num_points - 1)) * (2 * pi / num_points)
}

angles_to_df <- function(angles) {
  data.frame(x = cos(angles), y = sin(angles),
             color = angle_colors(angles),
             stringsAsFactors = FALSE)
}

# Data in a 2D circle, regularly spaced.
circle_data <- function(num_points) {
  angles_to_df(uniform_angle(num_points))
}

# Random points on a 2D circle.
random_circle_data <- function(num_points) {
  angles_to_df(runif(n = num_points, max = 2 * pi))
}

# Replicate each row in df n times
# http://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
replicate_rows <- function(df, n) {
  df[rep(seq_len(nrow(df)), each = n),]
}

# Clusters arranged in a circle.
random_circle_cluster_data <- function(num_points) {
  df <- replicate_rows(circle_data(num_points), n = 20)
  df$x <- df$x + rnorm(n = nrow(df), sd = 0.1)
  df$y <- df$y + rnorm(n = nrow(df), sd = 0.1)
  df
}

# Two 2D clusters of different sizes.
two_different_clusters_data_2D <- function(n) {
  cluster1 <- gaussian_data(n = n, dim = 2, sd = 10, color = '#003399')

  cluster2 <- gaussian_data(n = n, dim = 2, color = '#FF9900')
  cluster2[, 1] <- cluster2[, 1] + 100

  rbind(cluster1, cluster2)
}

# Two clusters of the same size.
two_clusters_data <- function(n, dim = 50) {
  cluster1 <- gaussian_data(n = n, dim = dim, color = '#003399')

  cluster2 <- gaussian_data(n = n, dim = dim, color = '#FF9900')
  cluster2[, 1] <- cluster2[, 1] + 10

  rbind(cluster1, cluster2)
}

# Two differently sized clusters, of arbitrary dimensions.
two_different_clusters_data <- function(n, dim = 50, scale = 10) {
  cluster1 <- gaussian_data(n = n, dim = dim, color = '#003399')

  cluster2 <- gaussian_data(n = n, dim = dim, sd = 1 / scale, color = '#FF9900')
  cluster2[, 1] <- cluster2[, 1] + 20

  rbind(cluster1, cluster2)
}

# Three clusters, at different distances from each other, in 2D
three_clusters_data_2d <- function(n) {
  three_clusters_data(n = n, dim = 2)
}

# Three clusters, at different distances from each other, in any dimension.
three_clusters_data <- function(n, dim = 50) {
  cluster1 <- gaussian_data(n = n, dim = dim, color = '#003399')

  cluster2 <- gaussian_data(n = n, dim = dim, color = '#FF9900')
  cluster2[, 1] <- cluster2[, 1] + 10

  cluster3 <- gaussian_data(n = n, dim = dim, color = '#66AA33')
  cluster3[, 1] <- cluster3[, 1] + 50

  rbind(cluster1, cluster2, cluster3)
}

# One tiny cluster inside of a big cluster.
subset_clusters_data <- function(n, dim = 2) {
  cluster1 <- gaussian_data(n = n, dim = dim, color = '#003399')
  cluster2 <- gaussian_data(n = n, dim = dim, sd = 50, color = '#FF9900')
  rbind(cluster1, cluster2)
}

# Data in a rough simplex.
simplex_data <- function(n, noise = 0) {
  df <- data.frame(matrix(0, nrow = n, ncol = n))
  for (i in 0:n - 1) {
    for (j in 0:n - 1) {
      if (i == j) {
        df[i, j] <- 1 + noise * rnorm(1)
      }
      else {
        df[i, j] <- 0
      }
    }
  }
  df
}

# Uniform points from a cube.
cube_data <- function(n, dim) {
  data.frame(matrix(runif(n * dim), nrow = n))
}


rotate <- function(df) {
  cs <- cos(.4)
  sn <- sin(.4)
  df$y <- cs * df$y + sn * df$z
  df$z <- -sn * df$y + cs * df$z
  df
}

# Points in two unlinked rings.
unlink_data <- function(n) {
  t <- uniform_angle(n)
  ring1 <- rotate(data.frame(x = cos(t), y = sin(t), z = 0, color = '#ff9900',
                             stringsAsFactors = FALSE))
  ring2 <- rotate(data.frame(x = 3 + cos(t), y = 0, z = sin(t),
                             color = '#003399', stringsAsFactors = FALSE))
  rbind(ring1, ring2)
}

# Points in linked rings.
link_data <- function(n) {
  t <- uniform_angle(n)
  ring1 <- rotate(data.frame(x = cos(t), y = sin(t), z = 0, color = '#ff9900',
                             stringsAsFactors = FALSE))
  ring2 <- rotate(data.frame(x = 1 + cos(t), y = 0, z = sin(t),
                             color = '#003399', stringsAsFactors = FALSE))
  rbind(ring1, ring2)
}

# Points in a trefoil knot.
trefoil_data <- function(n) {
  t <- uniform_angle(n)
  data.frame(
    x = sin(t) + 2 * sin(2 * t),
    y = cos(t) - 2 * cos(2 * t),
    z = -sin(3 * t),
    color = angle_colors(t),
    stringsAsFactors = FALSE
  )
}

# Two long, linear clusters in 2D.
long_cluster_data <- function(n) {
  s <- .03 * n;

  cluster1 <- data.frame(x = 0:(n - 1) + s * rnorm(n),
                         y = 0:(n - 1) + s * rnorm(n),
                         color = '#003399', stringsAsFactors = FALSE)

  cluster2 <- data.frame(x = 0:(n - 1) + s * rnorm(n) + n / 5,
                         y = 0:(n - 1) + s * rnorm(n) - n / 5,
                         color = '#ff9900', stringsAsFactors = FALSE)

  rbind(cluster1, cluster2)
}

# Mutually orthogonal steps.
ortho_curve <- function(n) {
  m <- matrix(0, nrow = n, ncol = n)
  m[lower.tri(m)] <- 1

  df <- data.frame(m)
  df$color <- angle_colors(1.5 * pi * 0:(n - 1) / n)
  df
}

# Random walk
random_walk <- function(n, dim) {
  current <- rep(0, dim)
  df <- data.frame(matrix(nrow = n, ncol = dim))
  for (i in 0:n - 1) {
    step <- rnorm(dim)
    next_step <- current
    for (j in 0:dim - 1) {
      next_step[j + 1] <- current[j + 1] + step[j + 1]
    }
    df[i + 1, ] <- next_step
    current <- next_step
  }

  df$color <- angle_colors(1.5 * pi * 0:(n - 1) / n)
  df
}

random_jump <- function(n, dim) {
  df <- data.frame(matrix(nrow = n, ncol = dim))
  current <- rep(0, dim)
  for (i in 0:n - 1) {
    step <- rnorm(dim)
    next_step <- step + current
    r <- rnorm(dim)
    r <- r * sqrt(dim)
    df[i + 1, ] <- r + next_step
    current <- next_step
  }

  df$color <- angle_colors(1.5 * pi * 0:(n - 1) / n)
  df
}
