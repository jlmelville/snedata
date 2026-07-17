#' Olivetti Faces dataset
#'
#' SNE benchmarking data.
#'
#' Returns the Olivetti Faces dataset in a data frame reformatted to have one
#' face per row, rather than column. This is a series of 400 images (with
#' dimensions 64 x 64) of 40 individuals' faces, with ten different poses per
#' person.
#'
#' The variables are as follows:
#' * `px1`, `px2`, `px3` ... `px4096` 8-bit grayscale
#'   pixel values (0-255). The pixel index starts at the top right of the image
#'   (`px1`) and are then stored column-wise.
#' * `Label`: An integer in the range (1-40) indicating the person.
#'
#' Each row has a name with the format `face_pose`, where `face` is the index
#' of the face, and `pose` is the index of the pose, e.g. the row with name
#' `20_10` is the tenth pose of the twentieth face.
#'
#' @note Requires the
#' [RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
#' package to be installed.
#' @return The Olivetti Faces dataset as a data frame.
#' @format A data frame with 400 rows and 4097 variables.
#' @seealso
#' * Sam Roweis' dataset web page: <http://www.cs.nyu.edu/~roweis/data.html>.
#' * Each row can be visualized as an image using [show_olivetti_face()].
#' @export
olivetti_faces <- function() {
  stop_if_not_installed("RnavGraphImageData")
  faces <- NULL
  utils::data("faces", package = "RnavGraphImageData", envir = environment())

  df <- as.data.frame(t(faces))
  npeople <- 40
  nposes <- 10
  colnames(df) <- sapply(seq(1, 4096), function(x) {
    paste0("px", x)
  })
  rownames(df) <- apply(
    expand.grid(seq(1, nposes), seq(1, npeople)),
    1,
    function(x) {
      paste(x[2], x[1], sep = "_")
    }
  )
  df$Label <- factor(as.numeric(cut(seq_len(nrow(df)), npeople)))

  df
}

#' Visualize Olivetti face.
#'
#' Display an image from the Olivetti faces dataset.
#'
#' @param df Data frame containing the Olivetti faces.
#' @param face Face index of the image to display. Must be an integer between
#' 1 and 40.
#' @param pose Pose index of the image to display. Must be an integer between
#' 1 and 10.
#' @param col List of colors to use in the display.
#' @export
show_olivetti_face <- function(
  df,
  face,
  pose,
  col = grDevices::gray(1 / 12:1)
) {
  if (
    !is.numeric(face) ||
      length(face) != 1 ||
      !is.finite(face) ||
      face < 1 ||
      face > 40 ||
      face != floor(face)
  ) {
    stop("face must be an integer between 1 and 40")
  }
  if (
    !is.numeric(pose) ||
      length(pose) != 1 ||
      !is.finite(pose) ||
      pose < 1 ||
      pose > 10 ||
      pose != floor(pose)
  ) {
    stop("pose must be an integer between 1 and 10")
  }
  n <- paste(face, pose, sep = "_")
  im <- t(matrix(as.numeric(df[n, 4096:1]), ncol = 64, nrow = 64))
  graphics::image(
    seq_len(nrow(im)),
    seq_len(nrow(im)),
    im,
    xlab = "",
    ylab = "",
    col = col
  )
}
