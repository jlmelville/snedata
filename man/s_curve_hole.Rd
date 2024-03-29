% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{s_curve_hole}
\alias{s_curve_hole}
\title{S-curve with a hole data set.}
\usage{
s_curve_hole(n_samples = 100, noise = 0)
}
\arguments{
\item{n_samples}{The number of points to create making up the S-shaped curve.
Fewer than \code{n_samples} points will be returned because some are
removed to make the hole.}

\item{noise}{Add random noise normally-distributed with mean 0 and standard
deviation \code{noise}.}
}
\value{
Data frame with \code{x}, \code{y}, \code{z} columns containing the
  coordinates of the points and \code{color} the RGB color.
}
\description{
Simulation data randomly sampled from an S-shaped curve with a hole.
}
\details{
Creates a series of points sampled from an S-shaped curve in 3D, with
optional normally-distributed noise. The S shape is oriented such that you
should be able to see it if you plot the X and Z columns. There is a circular
hole in the middle of the curve, centered at Y = 0.

Points are colored based on their distance along the curve.

This data set is based on \code{\link{s_curve}} is used to assess the
behavior of the PaCMAP method of Wang and co-workers (2021).
}
\references{
Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021).
Understanding how dimension reduction tools work: an empirical approach to
deciphering t-SNE, UMAP, TriMAP, and PaCMAP for data visualization.
\emph{J Mach. Learn. Res}, \emph{22}, 1-73.
}
\seealso{
the \href{https://github.com/YingfanWang/PaCMAP}{PaCMAP homepage}.
}
