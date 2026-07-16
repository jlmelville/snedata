#' Mammoth 10K
#'
#' Downloads the 10,000 point 'Mammoth' dataset, a 3D point cloud of a mammoth
#' skeleton.
#'
#' Downloads a dataframe containing the 10,000 3D coordinates of a mammoth
#' skeleton, digitized by
#' [the Smithsonian Institution](https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12).
#'
#' This dataset is from
#' [Understanding UMAP](https://pair-code.github.io/understanding-umap/),
#' based on work originally done by
#' [Max Noichl](https://github.com/MNoichl/UMAP-examples-mammoth-). It
#' consists of 10,000 points randomly sampled from the 50,000 point data set.
#'
#' @format A data frame with 10,000 rows and 3 variables, `X`, `Y`, and `Z`.
#'   The source coordinate vectors are ordered X, Z, Y; this function reorders
#'   them to the canonical X, Y, Z order. The Z-coordinate is the "height",
#'   i.e. the height of the mammoth varies with Z.
#'
#' For more information see <https://pair-code.github.io/understanding-umap/>.
#'
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return Data frame containing the Mammoth coordinates.
#' @note Requires the [rjson](https://cran.r-project.org/package=rjson)
#' package to be installed and loaded.
#' @seealso
#' * Max Noichl's page: <https://github.com/MNoichl/UMAP-examples-mammoth->.
#' * Understanding UMAP: <https://pair-code.github.io/understanding-umap/>.
#' * The Smithsonian page: <https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12>.
#' * [download_mammoth50k()] to download the original 50,000 points, from which this dataset was randomly sampled.
#' @examples
#' \dontrun{
#' mammoth <- download_mammoth10k()
#' # The mammoth in profile, facing left
#' plot(mammoth$X, mammoth$Z)
#' }
#' @export
download_mammoth10k <- function(timeout = 1800) {
  stop_if_not_installed("rjson")
  format_mammoth_coordinates(
    with_download_timeout(
      rjson::fromJSON(
        file = gh_raw(
          repo = "PAIR-code/understanding-umap",
          filename = "raw_data/mammoth_3d.json"
        )
      ),
      timeout = timeout
    ),
    source_order = c("X", "Z", "Y")
  )
}

#' Mammoth 50K
#'
#' Downloads the 50,000 point 'Mammoth' dataset, a 3D point cloud of a mammoth
#' skeleton.
#'
#' Downloads a dataframe containing the 50,000 3D coordinates of a mammoth
#' skeleton, digitized by
#' [the Smithsonian Institution](https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12).
#'
#' This dataset is from
#' [Understanding UMAP](https://pair-code.github.io/understanding-umap/),
#' based on work originally done by
#' [Max Noichl](https://github.com/MNoichl/UMAP-examples-mammoth-). 50,000
#' points were down-sampled from the raw data used by Max Noichl.
#'
#' @format A data frame with 50,000 rows and 3 variables, `X`, `Y`, and `Z`.
#'   The source coordinate vectors are ordered Y, X, Z; this function reorders
#'   them to the canonical X, Y, Z order. The Z-coordinate is the "height",
#'   i.e. the height of the mammoth varies with Z.
#'
#' For more information see <https://pair-code.github.io/understanding-umap/>.
#'
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return Data frame containing the Mammoth coordinates.
#' @note Requires the [rjson](https://cran.r-project.org/package=rjson)
#' package to be installed and loaded.
#' @seealso
#' * Max Noichl's page: <https://github.com/MNoichl/UMAP-examples-mammoth->.
#' * Understanding UMAP: <https://pair-code.github.io/understanding-umap/>.
#' * The Smithsonian page: <https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12>.
#' * [download_mammoth10k()] to download a 10,000 point random sub-sample of this data.
#' @examples
#' \dontrun{
#' mammoth <- download_mammoth50k()
#' # The mammoth in profile, facing left
#' plot(mammoth$X, mammoth$Z)
#' }
#' @export
download_mammoth50k <- function(timeout = 1800) {
  stop_if_not_installed("rjson")
  format_mammoth_coordinates(
    with_download_timeout(
      rjson::fromJSON(
        file = gh_raw(
          repo = "PAIR-code/understanding-umap",
          filename = "raw_data/mammoth_3d_50k.json"
        )
      ),
      timeout = timeout
    ),
    source_order = c("Y", "X", "Z")
  )
}

# The source data uses different coordinate orders for the two Mammoth assets.
# Keep the reordering local and testable without downloading either asset.
format_mammoth_coordinates <- function(coordinates, source_order) {
  df <- data.frame(do.call(rbind, coordinates))
  df <- df[, match(c("X", "Y", "Z"), source_order), drop = FALSE]
  names(df) <- c("X", "Y", "Z")
  df
}
