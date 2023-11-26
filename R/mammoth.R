#' Mammoth 10K
#'
#' Downloads the 10,000 point 'Mammoth' dataset, a 3D point cloud of a mammoth
#' skeleton.
#'
#' Downloads a dataframe containing the 10,000 3D coordinates of a mammoth
#' skeleton, digitized by
#' \href{https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12}{The Smithsonian Institute}.
#'
#' This dataset is from
#' \href{https://pair-code.github.io/understanding-umap/}{Understanding UMAP},
#' based on work originally done by
#' \href{https://github.com/MNoichl/UMAP-examples-mammoth-}{Max Noichl}. It
#' consists of 10,000 points randomly sampled from the 50,000 point data set.
#'
#' @format A data frame with 10,000 rows and 3 variables, \code{X}, \code{Z},
#'   \code{Y}, containing the X, Z, and Y coordinates respectively. This
#'   labeling of axes preserves the ordering of the data, and makes the
#'   Z-coordinate the "height", i.e. the height of the mammoth varies with Z.
#'   Note that the ordering of the axes differs from that of
#'   \code{\link{download_mammoth50k}}. Use the X, Y, and Z labels rather than
#'   numerical indices for consistency between the two datasets.
#'
#' For more information see \url{https://pair-code.github.io/understanding-umap/}.
#'
#' @return Data frame containing the Mammoth coordinates.
#' @note Requires the \href{https://cran.r-project.org/package=rjson}{rjson}
#' package to be installed and loaded.
#' @seealso
#' \itemize{
#' \item{Max Noichl's page: \url{https://github.com/MNoichl/UMAP-examples-mammoth-}.}
#' \item{Understanding UMAP: \url{https://pair-code.github.io/understanding-umap/}.}
#' \item{The Smithsonian page: \url{https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12}.}
#' \item{\code{\link{download_mammoth50k}} to download the original 50,000 points, from which this dataset was randomly sampled.}
#' }
#' @examples
#' \dontrun{
#' mammoth <- download_mammoth10k()
#' # The mammoth in profile, facing left
#' plot(mammoth$X, mammoth$Z)
#' }
#' @export
download_mammoth10k <- function() {
  stop_if_not_installed("rjson")
  df <- data.frame(do.call(rbind, rjson::fromJSON(
    file =
      gh_raw(repo = "PAIR-code/understanding-umap", filename = "raw_data/mammoth_3d.json")
  )))
  colnames(df) <- c("X", "Z", "Y")
  df
}

#' Mammoth 50K
#'
#' Downloads the 50,000 point 'Mammoth' dataset, a 3D point cloud of a mammoth
#' skeleton.
#'
#' Downloads a dataframe containing the 50,000 3D coordinates of a mammoth
#' skeleton, digitized by
#' \href{https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12}{The Smithsonian Institute}.
#'
#' This dataset is from
#' \href{https://pair-code.github.io/understanding-umap/}{Understanding UMAP},
#' based on work originally done by
#' \href{https://github.com/MNoichl/UMAP-examples-mammoth-}{Max Noichl}. 50,000
#' points were down-sampled from the raw data used by Max Noichl.
#'
#' @format A data frame with 50,000 rows and 3 variables, \code{Y}, \code{X},
#'   \code{Z}, containing the Y, X, and Z coordinates respectively. This
#'   labeling of axes preserves the ordering of the raw data, and makes the
#'   Z-coordinate the "height", i.e. the height of the mammoth varies with Z.
#'   Note that the ordering of the axes differs from that of
#'   \code{\link{download_mammoth10k}}. Use the X, Y, and Z labels rather than
#'   numerical indices for consistency between the two datasets.
#'
#' For more information see \url{https://pair-code.github.io/understanding-umap/}.
#'
#' @return Data frame containing the Mammoth coordinates.
#' @note Requires the \href{https://cran.r-project.org/package=rjson}{rjson}
#' package to be installed and loaded.
#' @seealso
#' \itemize{
#' \item{Max Noichl's page: \url{https://github.com/MNoichl/UMAP-examples-mammoth-}.}
#' \item{Understanding UMAP: \url{https://pair-code.github.io/understanding-umap/}.}
#' \item{The Smithsonian page: \url{https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12}.}
#' \item{\code{\link{download_mammoth10k}} to download a 10,000 point random sub-sample of this data.}
#' }
#' @examples
#' \dontrun{
#' mammoth <- download_mammoth50k()
#' # The mammoth in profile, facing left
#' plot(mammoth$X, mammoth$Z)
#' }
#' @export
download_mammoth50k <- function() {
  stop_if_not_installed("rjson")
  df <- data.frame(do.call(rbind, rjson::fromJSON(
    file =
      gh_raw(repo = "PAIR-code/understanding-umap", filename = "raw_data/mammoth_3d_50k.json")
  )))
  colnames(df) <- c("Y", "X", "Z")
  df
}
