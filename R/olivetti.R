#' Olivetti Faces dataset
#'
#' SNE benchmarking data.
#'
#' Returns the Olivetti Faces dataset in a data frame reformatted to have one
#' face per row, rather than column. This is a series of 400 images (with
#' dimension 64 x 64) of 40 individual's faces, with ten different poses per
#' person.
#'
#' The variables are as follows:
#' \itemize{
#' \item \code{px1}, \code{px2}, \code{px3} ... \code{px560} 8-bit grayscale
#' pixel values (0-255). The pixel index starts at the top right of the image
#' (\code{px1}) and are then stored column-wise.
#' \item \code{Label} An integer in the range (1-40) indicating the person.
#' }
#'
#' Each row has a name with the format "<face>_<pose>", where \code{<face>} is
#' the index of the face, and \code{<pose>} is the index of the pose, e.g.
#' the row with name \code{20_10} is the tenth pose of the twentieth face.
#'
#' @note requires the \code{RnavGraphImageData} package.
#' \url{https://cran.r-project.org/web/packages/RnavGraphImageData/index.html}
#' to be installed and loaded.
#' @return the Olivetti Faces dataset as a dataframe.
#' @format A data frame with 400 rows and 4097 variables.
#' @seealso
#' Saul Roweis' dataset web page: \url{http://www.cs.nyu.edu/~roweis/data.html}.
#' Each row can be visualized as an image using
#' \code{\link{show_olivetti_face}}.
#' @export
olivetti_faces <- function() {
  if (!requireNamespace("RnavGraphImageData", quietly = TRUE,
                        warn.conflicts = FALSE)) {
    stop("olivetti_faces function requires 'RnavGraphImageData' package")
  }
  faces <- NULL
  utils::data("faces", envir = environment())

  df <- as.data.frame(t(faces))
  npeople <- 40
  nposes <- 10
  colnames(df) <- sapply(seq(1, 4096), function(x) { paste0("px", x)})
  rownames(df) <- apply(expand.grid(seq(1, nposes), seq(1, npeople)), 1,
                        function(x) { paste(x[2], x[1], sep = "_") })
  df$Label <-  factor(as.numeric(cut(1:nrow(df), npeople)))

  df
}

#' Visualize Olivetti face.
#'
#' Display an image from the Olivetti faces dataset.
#'
#' @param df Data frame containing the Olivetti faces.
#' @param face Face index of the image to display. Must be an integer between
#' 1 and 400.
#' @param pose Pose index of the image to display. Must be an integer between
#' 1 and 10.
#' @param col List of colors to use in the display.
#' @export
show_olivetti_face <- function(df, face, pose,
                               col = grDevices::gray(1 / 12:1)) {
  if (face < 1 || face > 400) {
    stop("face must be an integer between 1 and 400")
  }
  if (pose < 1 || pose > 10) {
    stop("pose must be an integer between 1 and 10")
  }
  n <- paste(face, pose, sep = "_")
  im <- t(matrix(as.numeric(df[n, 4096:1]), ncol = 64, nrow = 64))
  graphics::image(1:nrow(im), 1:nrow(im), im, xlab = "", ylab = "", col = col)
}
