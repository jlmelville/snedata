% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misread-tsne.R
\name{subset_clusters_data}
\alias{subset_clusters_data}
\title{Subset Cluster Data}
\usage{
subset_clusters_data(n, dim = 2, big_sdev = 50)
}
\arguments{
\item{n}{Number of points per gaussian.}

\item{dim}{Dimension of the gaussians.}

\item{big_sdev}{Standard deviation of the bigger cluster, default 50. The
smaller cluster has a standard deviation of 1.}
}
\value{
Data frame with coordinates in the \code{X1}, \code{X2} ...
 \code{Xdim} columns, and color in the \code{color} column.
}
\description{
One tiny gaussian cluster inside of a big cluster from "How to Use t-SNE
Effectively".
}
\details{
Creates a dataset consisting of two gaussians with the same center, but
with the first cluster having a standard deviation of 1, and the second
having a standard deviation of \code{big_sdev} (default 50). Points are
colored depending on which cluster they belong to (small cluster is dark
powder blue, large is light orange).
}
\examples{
df <- subset_clusters_data(n = 50, dim = 2)

# 10D example where the big cluster is only twice the standard deviation of
# the small cluster
df <- subset_clusters_data(n = 50, dim = 10, big_sdev = 2)
}
\references{
\url{http://distill.pub/2016/misread-tsne/}
}
\seealso{
Other distill functions: 
\code{\link{circle_data}()},
\code{\link{cube_data}()},
\code{\link{gaussian_data}()},
\code{\link{grid_data}()},
\code{\link{link_data}()},
\code{\link{long_cluster_data}()},
\code{\link{long_gaussian_data}()},
\code{\link{ortho_curve}()},
\code{\link{random_circle_cluster_data}()},
\code{\link{random_circle_data}()},
\code{\link{random_jump}()},
\code{\link{random_walk}()},
\code{\link{simplex_data}()},
\code{\link{three_clusters_data}()},
\code{\link{trefoil_data}()},
\code{\link{two_clusters_data}()},
\code{\link{two_different_clusters_data}()},
\code{\link{unlink_data}()}
}
\concept{distill functions}
