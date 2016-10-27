#' Frey Faces dataset
#'
#' Image dataset.
#'
#' Returns the Frey Faces dataset in a data frame reformatted to be suitable
#' for use with Sneer. This is a series of 1965 images (with dimension 20 x 28)
#' of Brendan Frey's face taken from sequential frames of a video.
#'
#' The variables are as follows:
#' \itemize{
#' \item \code{px1}, \code{px2}, \code{px3} ... \code{px560} 8-bit grayscale
#' pixel values (0-255). The pixel index starts at the top right of the image
#' (\code{px1}) and are then stored row-wise.
#' \item \code{Label} An integer in the range (1-1965) indicating the frame.
#' Provides the same information as the row id, but included to be consistent
#' with other sneer datasets.
#' }
#'
#' @note requires the \code{RnavGraphImageData} package.
#' \url{https://cran.r-project.org/web/packages/RnavGraphImageData/index.html}
#' to be installed and loaded.
#' @return the Frey Faces dataset as a dataframe.
#' @format A data frame with 1965 rows and 561 variables.
#' @seealso
#' Saul Roweis' dataset web page: \url{http://www.cs.nyu.edu/~roweis/data.html}.
#' Each row can be visualized as an image using \code{\link{show_frey_face}}.
#' @export
frey_faces <- function() {
  if (!requireNamespace("RnavGraphImageData", quietly = TRUE,
                        warn.conflicts = FALSE)) {
    stop("frey_faces function requires 'RnavGraphImageData' package")
  }
  frey <- NULL
  utils::data("frey", envir = environment())

  df <- data.frame(t(frey))
  colnames(df) <- sapply(seq(1, 20 * 28), function(x) { paste0("px", x)})

  df$Label <- factor(as.numeric(cut(1:nrow(df), nrow(df))))
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
  graphics::image(1:nrow(im), 1:ncol(im), im, xlab = "", ylab = "", col = col)
}
