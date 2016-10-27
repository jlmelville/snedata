# Simulation data sets.

#' Sphere data set.
#'
#' Simulation data.
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
#' Simulation data.
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
#' Simulation data.
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
#' Simulation data.
#'
#' Creates a series of points randomly sampled from a swiss roll-shaped
#' manifold: a two-dimensional plane which has been rolled up into a spiral
#' shape. Or just look at a swiss roll.
#'
#' The formula for sampling the x, y and z coordinates used in this dataset is
#' from that given in the Stochastic Proximity Embedding paper by Agrafiotis
#' and Xu:
#' \deqn{x = \phi cos\phi, y = \phi sin \phi, z}
#' {x = phi * cos(phi), y = phi * sin(phi), z}
#'
#' where \eqn{\phi}{phi} and z are random numbers in the intervals [5, 13]
#' and [0, 10], respectively.
#'
#' Points are colored based on the value of phi. If you unrolled the manifold
#' into a flat sheet, you would see the color change linearly in the direction
#' you unrolled it. Or just plot the x-y cross section (see the examples).
#'
#' @param n Number of points to create.
#' @return Data frame with \code{x}, \code{y}, \code{z} columns containing the
#'  coordinates of the points and \code{color} the RGB color.
#' @references
#' Agrafiotis, D. K., & Xu, H. (2002).
#' A self-organizing principle for learning nonlinear manifolds.
#' \emph{Proceedings of the National Academy of Sciences}, \emph{99}(25), 15869-15872.
#' @examples
#' \dontrun{
#'  swiss1000 <- swiss_roll(n = 1000)
#'  # Coloring should be obvious with a 2D plot of the cross section:
#'  plot(swiss1000$x, swiss1000$y, col = swiss1000$color, pch = 20)
#' }
#' @export
swiss_roll <- function(n = 1000) {
  phi <- stats::runif(n, min = 5, max = 30)
  x <- phi * cos(phi)
  y <- phi * sin(phi)
  z <- stats::runif(n, max = 10)

  data.frame(x, y, z, color = linear_color_map(phi), stringsAsFactors = FALSE)
}
