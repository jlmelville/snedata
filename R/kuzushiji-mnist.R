# Base URL of the Kuzushiji MNIST dataset website
kuzushiji_mnist_url <-
  "http://codh.rois.ac.jp/kmnist/dataset/kmnist/"

#' Download Kuzushiji-MNIST
#'
#' Download Kuzushiji-MNIST database of images of cursive Japanese writing.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame. The dataset is intended to be a drop-in
#' replacement for the MNIST digits dataset.
#'
#' @format A data frame with 786 variables:
#'
#' \describe{
#' \item{\code{px1}, \code{px2}, \code{px3} ... \code{px784}}{Integer pixel
#'   value, from 0 (white) to 255 (black).}
#' \item{\code{Label}}{The character, represented by an integer in the 
#' range 0-9.}
#' }
#'
#' Pixels are organized row-wise. The \code{Label} variable is stored as a
#' factor.
#'
#' There are 70,000 items in the data set. The first 60,000 are the training
#' set, as found in the \code{train-images-idx3-ubyte.gz} file. The remaining
#' 10,000 are the test set, from the \code{t10k-images-idx3-ubyte.gz} file.
#'
#' Items in the dataset can be visualized with the
#' \code{\link{show_mnist_digit}} function.
#'
#' For more information see
#' \url{https://github.com/rois-codh/kmnist}.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing Kuzushiji-MNIST.
#' @note Originally based on a function by Brendan O'Connor.
#' @export
#' @examples
#' \dontrun{
#' # download the data set
#' kuzushiji <- download_kuzushiji_mnist()
#'
#' # first 60,000 instances are the training set
#' kuzushiji_train <- head(kuzushiji, 60000)
#' # the remaining 10,000 are the test set
#' kuzushiji_test <- tail(kuzushiji, 10000)
#'
#' # PCA on 1000 examples
#' kuzushiji_r1000 <- kuzushiji[sample(nrow(kuzushiji), 1000), ]
#' pca <- prcomp(kuzushiji_r1000[, 1:784], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = 'n')
#' text(pca$x[, 1:2], labels = kuzushiji_r1000$Label,
#'      col = rainbow(length(levels(kuzushiji_r1000$Label)))[kuzushiji$Label])
#'}
#' @references
#' "KMNIST Dataset" (created by CODH), adapted from "Kuzushiji Dataset" (created by NIJL and others), 
#' doi:10.20676/00000341
#' \url{https://github.com/rois-codh/kmnist}
#' 
#' Clanuwat, T., Bober-Irizar, M., Kitamoto, A., Lamb, A., Yamamoto, K., & Ha, D. (2018). 
#' Deep Learning for Classical Japanese Literature. 
#' \emph{arXiv preprint} \emph{arXiv:1812.01718}.
#' 
#' @export
download_kuzushiji_mnist <- function(base_url = kuzushiji_mnist_url,
                                   verbose = FALSE) {
  res <- download_mnist(base_url = base_url, verbose = verbose)

  res
}
