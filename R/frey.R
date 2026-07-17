#' Frey Faces dataset
#'
#' Converts Frey Faces Image dataset.
#'
#' Returns the Frey Faces dataset in a data frame where each row is one image.
#' This is a series of 1965 images (with dimension 20 x 28) of Brendan Frey's
#' face taken from sequential frames of a video.
#'
#' The variables are as follows:
#' * `px1`, `px2`, `px3` ... `px560` 8-bit grayscale
#'   pixel values (0-255). The pixel index starts at the top right of the image
#'   (`px1`) and are then stored row-wise.
#' * `color`: A string representing a color in hex format. It can be
#'   used directly with e.g. the `col` parameter in the
#'   [graphics::plot()] function. The color goes from hsl(0, 50, 50)
#'   (red) at frame 1 to hsl(300, 50, 50) (purple) at frame 1965 and on a muted
#'   rainbow scale.
#'
#' @note Requires the
#' [RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
#' package to be installed.
#' @return The Frey Faces dataset as a data frame.
#' @format A data frame with 1965 rows and 561 variables.
#' @seealso
#' * Sam Roweis' dataset web page: <http://www.cs.nyu.edu/~roweis/data.html>.
#' * Each row can be visualized as an image using [show_frey_face()].
#' @export
#' @examples
#' \dontrun{
#' frey <- frey_faces()
#' # PCA Scores plot, with color indicating the frame index
#' frey_pca <- prcomp(frey[, -561], retx = TRUE, rank. = 2)
#' plot(frey_pca$x, col = frey$color, pch = 16, cex = 0.75)
#' }
frey_faces <- function() {
  stop_if_not_installed("RnavGraphImageData")
  frey <- NULL
  utils::data("frey", package = "RnavGraphImageData", envir = environment())

  df <- data.frame(t(frey))
  colnames(df) <- sapply(seq(1, 20 * 28), function(x) {
    paste0("px", x)
  })

  df$color <- linear_color_map(1:1965)
  df
}

#' Visualize Frey face.
#'
#' Display an image from the Frey faces dataset.
#'
#' @param df Data frame containing the Frey faces.
#' @param n Frame index of the image to display.
#' @param col List of colors to use in the display.
#' @export
show_frey_face <- function(df, n, col = grDevices::gray(1 / 12:1)) {
  if (n < 1 || n > nrow(df)) {
    stop("n must be value between 1 and ", nrow(df))
  }
  im <- matrix(t(df[n, 560:1]), ncol = 28, nrow = 20)
  graphics::image(
    seq_len(nrow(im)),
    seq_len(ncol(im)),
    im,
    xlab = "",
    ylab = "",
    col = col
  )
}
