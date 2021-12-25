# Simulation data sets.

#' Sphere data set.
#'
#' Simulation data randomly sampled from the surface of a 3D sphere.
#'
#' Creates a series of points sampled from a 3D spherical surface.
#'
#' Points are colored based on the angle theta in their spherical coordinate
#' presentation. You can think of it as dividing the sphere surface like the
#' segments of an orange.
#'
#' @param n The number of points to create.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' A dataset like this was used in:
#' Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015).
#' Multi-scale similarities in stochastic neighbour embedding: Reducing
#' dimensionality while preserving both local and global structure.
#' \emph{Neurocomputing}, \emph{169}, 246-261.
#' @export
sphere <- function(n = 1000) {
  # from http://stats.stackexchange.com/questions/7977/how-to-generate-uniformly-distributed-points-on-the-surface-of-the-3-d-unit-sphe
  z <- stats::runif(n, min = -1, max = 1)
  theta <- stats::runif(n, min = -pi, max = pi)
  x <- sin(theta) * sqrt(1 - z ^ 2)
  y <- cos(theta) * sqrt(1 - z ^ 2)

  data.frame(x, y, z, color = linear_color_map(theta + pi),
             stringsAsFactors = FALSE)
}

#' Ball Data Set
#'
#' Simulation data randomly sampled from the entire volume of a 3D sphere.
#'
#' Creates a series of points sampled from a 3D spherical volume. Points are
#' colored based on the square of their distance from the origin.
#'
#' @param n Number of points to create.
#' @param rad Radius of the ball.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' A dataset like this was used in:
#' Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015).
#' Multi-scale similarities in stochastic neighbour embedding: Reducing
#' dimensionality while preserving both local and global structure.
#' \emph{Neurocomputing}, \emph{169}, 246-261.
#' @export
ball <- function(n = 1000, rad = 1) {
  # from http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
  u <- stats::runif(n)
  xyz <- matrix(stats::rnorm(3 * n), nrow = n,
                dimnames = list(NULL, c("x", "y", "z")))

  df <- data.frame((xyz * rad * u ^ (1 / 3)) / sqrt(rowSums(xyz ^ 2)))
  dist2 <- rowSums(df ^ 2)
  df$color <- linear_color_map(dist2)
  df
}

#' Toroidal Helix Data Set
#'
#' Simulation data in the shape of a 3D toroidal helix.
#'
#' Creates a series of points sampled from a 3D helix with the ends joined
#' to each other.
#'
#' Unlike \code{\link{ball}} and \code{\link{sphere}}, this data set is not
#' randomly sampled.
#'
#' Points are colored based on their distances from the origin.
#'
#' @param n Number of points to create.
#' @param rmajor Major radius.
#' @param rminor Minor radius.
#' @param nwinds Number of winds the helix makes.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' A dataset like this was used in:
#' Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015).
#' Multi-scale similarities in stochastic neighbour embedding: Reducing
#' dimensionality while preserving both local and global structure.
#' \emph{Neurocomputing}, \emph{169}, 246-261.
#' @examples
#' \dontrun{
#'  helix1000 <- helix(n = 1000)
#'  # Coloring should be obvious with a 2D plot of the cross section:
#'  plot(helix1000$x, helix1000$y, col = helix1000$color, pch = 20)
#' }
#' @export
helix <- function(n = 1000, rmajor = 2, rminor = 1, nwinds = 8) {
  # http://math.stackexchange.com/questions/324527/do-these-equations-create-a-helix-wrapped-into-a-torus
  u <- seq(-pi, pi, length.out = n)
  w <- rmajor + (rminor * cos(nwinds * u))
  x <- w * cos(u)
  y <- w * sin(u)
  z <- rminor * sin(nwinds * u)

  df <- data.frame(x, y, z)

  dist <- sqrt(rowSums(df ^ 2))
  df$color <- linear_color_map(dist)

  df
}

#' Swiss Roll Data Set
#'
#' Simulation data randomly sampled from a 2D plane curled up into 3 dimensions.
#'
#' Creates a series of points randomly sampled from a swiss roll-shaped
#' manifold: a two-dimensional plane which has been rolled up into a spiral
#' shape. Or just look at a swiss roll.
#'
#' The formula for sampling the x, y and z coordinates used in this dataset is
#' from that given in the Stochastic Proximity Embedding paper by Agrafiotis
#' and Xu (I don't know who originally came up with the data set, though):
#' \deqn{x = \phi cos\phi, y = \phi sin \phi, z}{x = phi * cos(phi), y = phi * sin(phi), z}
#'
#' where \eqn{\phi}{phi} and z are random numbers in the intervals
#' [\eqn{\frac{3\pi}{2}}{3/2 pi}, \eqn{\frac{5\pi}{2}}{5/2 pi}]
#' and [0, 10], respectively (the range of \eqn{\phi} and \code{z} can
#' be modified, if desired).
#'
#' Points are colored based on the value of phi. If you unrolled the manifold
#' into a flat sheet, you would see the color change linearly in the direction
#' you unrolled it. Or just plot the x-y cross section (see the examples).
#'
#' @param n Number of points to create.
#' @param min_phi Minimum value of the range of \eqn{phi}{phi} to sample from
#' @param max_phi Maximum value of the range of \eqn{phi}{phi} to sample from.
#' @param max_z Maximum value of the \code{z} range to sample from (minimum
#'  values is always 0).
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' I first saw the equations in
#' 
#' Agrafiotis, D. K., & Xu, H. (2002).
#' A self-organizing principle for learning nonlinear manifolds.
#' \emph{Proceedings of the National Academy of Sciences}, \emph{99}(25), 15869-15872.
#'
#' But the dataset turns up everywhere, most notably:
#' 
#' Roweis, S. T., & Saul, L. K. (2000). 
#' Nonlinear dimensionality reduction by locally linear embedding. 
#' \emph{Science}, \emph{290}(5500), 2323-2326.
#'
#' If the idea of flattening a Swiss Roll didn't originate in that publication,
#' it was certainly popularized for its use in nonlinear dimensionality
#' reduction. A Matlab-formatted version of that dataset is still available at:
#' 
#' \url{http://web.mit.edu/cocosci/isomap/datasets.html}
#' 
#' I'm not sure exactly what parameters were used to generate it, but you
#' can get something similar by calling:
#' \code{swiss_roll(n = 20000, min_phi = 1.5 * pi, max_phi = 4.5 * pi, max_z = 50)}
#'
#' @examples
#' \dontrun{
#'  swiss1000 <- swiss_roll(n = 1000)
#'  # Coloring should be obvious with a 2D plot of the cross section:
#'  plot(swiss1000$x, swiss1000$y, col = swiss1000$color, pch = 20)
#' }
#' @export
swiss_roll <- function(n = 1000, min_phi = 1.5 * pi, max_phi = 4.5 * pi,
                       max_z = 10) {
  phi <- stats::runif(n, min = min_phi, max = max_phi)
  x <- phi * cos(phi)
  y <- phi * sin(phi)
  z <- stats::runif(n, max = max_z)

  data.frame(x, y, z, color = linear_color_map(phi), stringsAsFactors = FALSE)
}


#' S-curve data set.
#'
#' Simulation data randomly sampled from an S-shaped curve. Translated from the
#' \href{https://scikit-learn.org/stable/index.html}{scikit-learn} Pythom 
#' function \code{sklearn.datasets.make_s_curve}.
#'
#' Creates a series of points sampled from an S-shaped curve in 3D, with
#' optional normally-distributed noise. The S shape is oriented such that you
#' should be able to see it if you plot the X and Z columns.
#' 
#' Points are colored based on their distance along the curve.
#' 
#' @param n_samples The number of points to create.
#' @param noise Add random noise normally-distributed with mean 0 and standard 
#'   deviation \code{noise}.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' Buitinck, L., Louppe, G., Blondel, M., Pedregosa, F., Mueller, A., 
#' Grisel, O., ... & Varoquaux, G. (2013). 
#' API design for machine learning software: experiences from the scikit-learn 
#' project. 
#' \emph{arXiv preprint} \emph{arXiv:1309.0238}.
#' @export
s_curve <- function(n_samples = 100, noise = 0.0) {
  tt <- 3 * pi * stats::runif(n = n_samples, min = -0.5, max = 0.5)
  x <- sin(tt)
  y <- 2.0 * stats::runif(n = n_samples)
  z <- sign(tt) * (cos(tt) - 1)
  X <- cbind(x, y, z)
  X <- X + noise * stats::rnorm(n_samples * 3)
  
  data.frame(X, color = linear_color_map(tt), stringsAsFactors = FALSE)
}

#' S-curve with a hole data set.
#'
#' Simulation data randomly sampled from an S-shaped curve with a hole.
#'
#' Creates a series of points sampled from an S-shaped curve in 3D, with
#' optional normally-distributed noise. The S shape is oriented such that you
#' should be able to see it if you plot the X and Z columns. There is a circular
#' hole in the middle of the curve, centered at Y = 0.
#'  
#' Points are colored based on their distance along the curve.
#' 
#' This data set is based on \code{\link{s_curve}} is used to assess the
#' behavior of the PaCMAP method of Wang and co-workers (2021).
#' 
#' @param n_samples The number of points to create making up the S-shaped curve.
#'   Fewer than \code{n_samples} points will be returned because some are
#'   removed to make the hole.
#' @param noise Add random noise normally-distributed with mean 0 and standard
#'   deviation \code{noise}.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'   coordinates of the points and \code{color} the RGB color.
#' @references
#' Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021). 
#' Understanding how dimension reduction tools work: an empirical approach to 
#' deciphering t-SNE, UMAP, TriMAP, and PaCMAP for data visualization. 
#' \emph{J Mach. Learn. Res}, \emph{22}, 1-73.
#' @seealso the \href{https://github.com/YingfanWang/PaCMAP}{PaCMAP homepage}.
#' @export
s_curve_hole <- function(n_samples = 100, noise = 0.0) {
  scurve <- s_curve(n_samples = n_samples, noise = noise)
  X <- scurve[, 1:3]

  anchor <- c(0, 1, 0)
  indices <- rowSums((sweep(X, 2, anchor, `-`)) ^ 2) > 0.3
  scurve <- scurve[indices, ]
  rownames(scurve) <- NULL
  scurve
}
