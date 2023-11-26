# Base URL of the Fashion MNIST dataset website
fashion_mnist_url <-
  "http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/"

#' Download Fashion-MNIST
#'
#' Download Fashion-MNIST database of images of fashion products.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame. The dataset is intended to be a drop-in
#' replacement for the MNIST digits dataset but with more relevance for
#' benchmarking machine learning algorithms (i.e. it's more difficult).
#'
#' @format A data frame with 786 variables:
#'
#' \describe{
#' \item{\code{px1}, \code{px2}, \code{px3} ... \code{px784}}{Integer pixel
#'   value, from 0 (white) to 255 (black).}
#' \item{\code{Label}}{The fashion item represented by the image, in the range
#'  0-9.}
#' \item{\code{Description}}{The name of the fashion item associated with the
#'  \code{Label}}
#' }
#'
#' Pixels are organized row-wise. The \code{Label} variable is stored as a
#' factor. The labels correspond to:
#'
#' \describe{
#'   \item{\code{0}}{T-shirt/top}
#'   \item{\code{1}}{Trouser}
#'   \item{\code{2}}{Pullover}
#'   \item{\code{3}}{Dress}
#'   \item{\code{4}}{Coat}
#'   \item{\code{5}}{Sandal}
#'   \item{\code{6}}{Shirt}
#'   \item{\code{7}}{Sneaker}
#'   \item{\code{8}}{Bag}
#'   \item{\code{9}}{Ankle boot}
#' }
#'
#' and are also present as the \code{Description} factor.
#'
#' There are 70,000 items in the data set. The first 60,000 are the training
#' set, as found in the \code{train-images-idx3-ubyte.gz} file. The remaining
#' 10,000 are the test set, from the \code{t10k-images-idx3-ubyte.gz} file.
#'
#' Items in the dataset can be visualized with the
#' \code{\link{show_mnist_digit}} function.
#'
#' For more information see
#' \url{https://github.com/zalandoresearch/fashion-mnist}.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing Fashion-MNIST.
#' @note Originally based on a function by Brendan O'Connor.
#' @examples
#' \dontrun{
#' # download the data set
#' fashion <- download_fashion_mnist()
#'
#' # first 60,000 instances are the training set
#' fashion_train <- head(fashion, 60000)
#' # the remaining 10,000 are the test set
#' fashion_test <- tail(fashion, 10000)
#'
#' # PCA on 1000 examples
#' fashion_r1000 <- fashion[sample(nrow(fashion), 1000), ]
#' pca <- prcomp(fashion_r1000[, 1:784], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = fashion_r1000$Label,
#'   col = rainbow(length(levels(fashion$Label)))[fashion_r1000$Label]
#' )
#' }
#' @references
#' Xiao, H., Kashif, R., & Vollgraf, R. (2017).
#' Fashion-MNIST: a Novel Image Dataset for Benchmarking Machine Learning Algorithms.
#' \emph{arXiv preprint} \emph{arXiv:1708.07747}.
#' \url{https://github.com/zalandoresearch/fashion-mnist/}
#' @export
download_fashion_mnist <- function(base_url = fashion_mnist_url,
                                   verbose = FALSE) {
  res <- download_mnist(base_url = base_url, verbose = verbose)

  description_levels <- c(
    "T-shirt/top", "Trouser", "Pullover", "Dress",
    "Coat", "Sandal", "Shirt", "Sneaker", "Bag",
    "Ankle boot"
  )
  res$Description <- factor(description_levels[as.numeric(res$Label)],
    levels = description_levels
  )
  res
}
