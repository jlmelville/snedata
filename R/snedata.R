#' snedata: Dataset functions for Stochastic Neighbor Embedding and related
#' dimensionality reduction methods.
#' 
#' Functions to download or generate some datasets commonly used for
#' benchmarking various dimensionality reduction methods.
#'
#' @section Simulation functions:
#' Very simple datasets for characterizing the behavior of various embedding
#' algorithms some, from Lee et al (2016) and Agrafiotis and Xu (2002).
#'
#' \itemize{
#'  \item{\code{\link{swiss_roll}}. A 2D plane curled into 3D. }
#'  \item{\code{\link{sphere}}. Points sampled from the surface of a 3D sphere.}
#'  \item{\code{\link{ball}}. Points sampled from the interior of a 3D sphere.}
#'  \item{\code{\link{helix}}. Points sampled from a 3D toroidal helix with
#'    the ends jointed together.}
#' }
#'
#' @section Simulation functions from "How to use t-SNE Effectively":
#' An online article from Wattenberg et al (2016), this runs t-SNE live in your
#' browser. Much of the data pertains to investigating the behavior of clusters
#' of gaussians of varying dimensionality, heterogeneity and relative separation.
#' There's quite a lot of functions:
#' \itemize{
#'  \item{\code{\link{circle_data}}. A 2D circle.}
#'  \item{\code{\link{cube_data}}. A cube.}
#'  \item{\code{\link{gaussian_data}}. A Gaussian.}
#'  \item{\code{\link{grid_data}}. A 2D grid.}
#'  \item{\code{\link{link_data}}. Two linked circles.}
#'  \item{\code{\link{long_cluster_data}}. Two long parallel clusters.}
#'  \item{\code{\link{long_gaussian_data}}. A gaussian with unequal standard
#'    deviation in its dimensions.}
#'  \item{\code{\link{ortho_curve}}. Points related by mutually orthogonal
#'    steps.}
#'  \item{\code{\link{random_circle_cluster_data}}. Points sampled from a
#'    circle and jittered.}
#'  \item{\code{\link{random_circle_cluster_data}}. Points randomly sampled
#'    from the circumference of a circle.}
#'  \item{\code{\link{random_circle_cluster_data}}. Points generated from a
#'    random walk with an extra gaussian perturbation.}
#'  \item{\code{\link{random_walk}}. Points generated from a random walk.}
#'  \item{\code{\link{simplex_data}}. Points arranged as a rough simplex.}
#'  \item{\code{\link{subset_clusters_data}}. A small cluster inside a larger
#'    one.}
#'  \item{\code{\link{three_clusters_data}}. Points sampled from three gaussian
#'    clusters with unequal relative distances.}
#'  \item{\code{\link{trefoil_data}}. Points sampled in the shape of 3D trefoil
#'    knot.}
#'  \item{\code{\link{two_clusters_data}}. Two gaussian clusters.}
#'  \item{\code{\link{two_different_clusters_data}}. Two gaussian clusters,
#'    with differing standard deviations.}
#'  \item{\code{\link{unlink_data}}. Two unlinked circles.}
#' }
#'
#'
#' @section Faces dataset functions:
#' If you have the \code{RnavGraphImageData} package
#' \url{https://cran.r-project.org/package=RnavGraphImageData}
#' installed and loaded, then there are functions to convert the Olivetti
#' and Frey faces datasets into a row-based format, and functions to visualize
#' the images.
#'
#' \itemize{
#'  \item{\code{\link{frey_faces}}. Loads the Frey faces as a row-based data
#'    frame.}
#'  \item{\code{\link{show_frey_face}}. Display one of the poses from the Frey
#'    faces.}
#'  \item{\code{\link{olivetti_faces}}. Loads the Olivetti faces as a row-based
#'    data frame.}
#'  \item{\code{\link{show_olivetti_face}}. Display one of the poses from the
#'    Olivetti faces.}
#' }
#'
#' @section MNIST dataset functions:
#' Another (much larger) image dataset is the MNIST digits dataset, available
#' from \url{http://yann.lecun.com/exdb/mnist/}. Functions are available to
#' download the dataset as a data frame and visualize individual digits:
#'
#' \itemize{
#'   \item{\code{\link{download_mnist}}. Downloads the MNIST dataset files
#'   as a row-based data frame.}
#'   \item{\code{\link{show_mnist_digit}}. Display one of the MNIST digits.}
#' }
#'
#' @section Fashion-MNIST functions:
#' The Fashion-MNIST dataset (Xiao et al. 2017) is designed as a drop-in
#' replacement
#'
#' @note
#' The faces datasets originate from Sam Roweis' dataset web page:
#' \url{http://www.cs.nyu.edu/~roweis/data.html}.
#'
#' Code to download and visualize the MNIST digits and fashion datasets
#' originates from a gist by Brendan O'Connor:
#' \url{https://gist.github.com/brendano/39760}.
#'
#' @examples
#'  # 300 points sampled from the surface of a sphere
#'  sphere300 <- sphere(n = 300)
#'
#'  # 150 points sampled from a toroidal helix with 30 coils:
#'  helix150 <- helix(n = 150, nwinds = 30)
#'
#'  # 150 points from a filled sphere:
#'  ball150 <- ball(n = 150)
#'
#'  # 100 points from a "Swiss Roll" distribution:
#'  swiss100 <- swiss_roll(n = 100)
#'
#'  # 50 points from a 2D gaussian
#'  g2d <- gaussian_data(n = 50, dim = 2)
#'
#' \dontrun{
#'  # Load RnavGraphImageData
#'  library(RnavGraphImageData)
#'
#'  # Load the Frey faces dataset with each image as a row
#'  frey <- frey_faces()
#'  # Display the first pose
#'  show_frey_face(frey, 1)
#'
#'  # Load the Olivetti faces dataset with each image as a row
#'  olivetti <- olivetti_faces()
#'  # Show the second pose of the first face
#'  show_olivetti_face(olivetti, 1, 2)
#' }
#'
#' \dontrun{
#' # fetch the MNIST data set from the MNIST website
#' mnist <- download_mnist()
#' # view the fifth digit
#' show_mnist_digit(mnist, 5)
#'
#' # first 60,000 instances are the training set
#' mnist_train <- head(mnist, 60000)
#' # the remaining 10,000 are the test set
#' mnist_test <- tail(mnist, 10000)
#'
#' # PCA on 1000 random training examples
#' mnist_r1000 <- mnist_train[sample(nrow(mnist_train), 1000), ]
#'
#' pca <- prcomp(mnist_r1000[, 1:784], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = 'n')
#' text(pca$x[, 1:2], labels = mnist_r1000$Label, cex = 0.5,
#'      col = rainbow(length(levels(mnist_r1000$Label)))[mnist_r1000$Label])
#'
#' # save to disk
#' save(mnist, file = "mnist.Rda")
#' }
#'
#' \dontrun{
#' # fetch the Fashion-MNIST data
#' fashion <- download_fashion_mnist()
#'
#' # You can repeat the same example as with the MNIST digits example.
#' # Shows the fifth Fashion-MNIST "digit" (actually a dress)
#' show_mnist_digit(fashion, 5)
#'
#' # and so on...
#' }
#' @references
#'
#' Agrafiotis, D. K., & Xu, H. (2002).
#' A self-organizing principle for learning nonlinear manifolds.
#' \emph{Proceedings of the National Academy of Sciences}, \emph{99}(25), 15869-15872.
#'
#' Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015).
#' Multi-scale similarities in stochastic neighbour embedding: Reducing
#' dimensionality while preserving both local and global structure.
#' \emph{Neurocomputing}, \emph{169}, 246-261.
#'
#' Wattenberg, M., Vie'gas, F., & Johnson, I. (2016)
#' How to Use t-SNE Effectively.
#' \emph{Distill}
#' \url{http://doi.org/10.23915/distill.00002}
#'
#' Xiao, H., Kashif, R., & Vollgraf, R. (2017).
#' Fashion-MNIST: a Novel Image Dataset for Benchmarking Machine Learning Algorithms.
#' \emph{arXiv preprint} \emph{arXiv:1708.07747}.
#' \url{https://github.com/zalandoresearch/fashion-mnist/}
#'
#' @docType package
#' @name snedata
NULL
