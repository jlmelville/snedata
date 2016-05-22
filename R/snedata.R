#' snedata: Dataset functions for Stochastic Neighbor Embedding and related
#' dimensionality reduction methods.
#'
#' @section Simulation functions:
#' Very simple datasets for characterizing the behavior of various embedding
#' algorithms:
#'
#' \itemize{
#'  \item{\code{\link{gauss}} Data distributed according to a Gaussian
#'    distribution.}
#'  \item{\code{\link{swiss_roll}}. Two 2D planes curled in a spiral.}
#'  \item{\code{\link{sphere}}. Points sampled from the surface of a 3D sphere.}
#'  \item{\code{\link{ball}}. Points sampled from the interior of a 3D sphere.}
#'  \item{\code{\link{helix}}. Points sampled from a 3D toroidal helix with
#'    the ends jointed together.}
#' }
#'
#' @section Faces dataset functions:
#' If you have the \code{RnavGraphImageData} package
#' \url{https://cran.r-project.org/web/packages/RnavGraphImageData/index.html}
#' installed and loaded, then there are functions to convert the Olivetti
#' and Frey faces datasets into a row-based format, and functions to visualize
#' the images.
#'
#' \itemize{
#'  \item{\code{\link{frey_faces}} Loads the Frey faces as a row-based data
#'    frame.}
#'  \item{\code{\link{show_frey_face}}. Display one of the poses from the Frey
#'    faces.}
#'  \item{\code{\link{olivetti_faces}} Loads the Olivetti faces as a row-based
#'    data frame.}
#'  \item{\code{\link{show_olivetti_face}}. Display one of the poses from the
#'    Olivetti faces.}
#' }
#'
#' @note
#' The faces datasets originate from Saul Roweis' dataset web page:
#' \url{http://www.cs.nyu.edu/~roweis/data.html}.
#'
#' @examples
#'  # 3000 points sampled from the surface of a sphere
#'  sphere3000 <- sphere(n = 3000)
#'
#'  # 1500 points sampled from a toroidal helix with 30 coils:
#'  helix1500 <- helix(n = 1500, nwinds = 30)
#'
#'  # 1500 points from a filled sphere:
#'  ball1500 <- ball(n = 1500)
#'
#'  # 1000 points from a five-dimensional gaussian:
#'  gauss1000 <- gauss(n = 1000, d = 5)
#'
#'  # 1000 points from a "Swiss Roll" distribution:
#'  swiss1000 <- swiss_roll(n = 1000)
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
#' @references
#' Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015).
#' Multi-scale similarities in stochastic neighbour embedding: Reducing
#' dimensionality while preserving both local and global structure.
#' \emph{Neurocomputing}, \emph{169}, 246-261.
#'
#' Agrafiotis, D. K., & Xu, H. (2002).
#' A self-organizing principle for learning nonlinear manifolds.
#' \emph{Proceedings of the National Academy of Sciences}, \emph{99}(25), 15869-15872.
#' @docType package
#' @name snedata
NULL
