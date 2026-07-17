# Base URL of the Kuzushiji MNIST dataset website
kuzushiji_mnist_url <-
  "https://codh.rois.ac.jp/kmnist/dataset/kmnist/"

#' Download Kuzushiji-MNIST
#'
#' Download Kuzushiji-MNIST database of images of cursive Japanese writing.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame or canonical image result. The dataset is
#' intended to be a drop-in replacement for the MNIST digits dataset.
#'
#' @format A data frame with 785 variables:
#'
#' * `px1`, `px2`, `px3` ... `px784`: Integer pixel
#'   value, from 0 (white) to 255 (black).
#' * `Label`: The character, represented by an integer in the
#'   range 0-9.
#'
#' Pixels are organized row-wise. The `Label` variable is stored as a
#' factor.
#'
#' There are 70,000 items in the data set. The first 60,000 are the training
#' set, as found in the `train-images-idx3-ubyte.gz` file. The remaining
#' 10,000 are the test set, from the `t10k-images-idx3-ubyte.gz` file.
#'
#' Items in the dataset can be visualized with the
#' [show_mnist_digit()] function.
#'
#' For more information see
#' <https://github.com/rois-codh/kmnist>.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"list"` for the canonical image result described in
#'   [download_mnist()].
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return A data frame containing Kuzushiji-MNIST, or a canonical image result
#'   with factor labels and explicit split identity in `meta`.
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
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = kuzushiji_r1000$Label,
#'   col = rainbow(length(levels(kuzushiji_r1000$Label)))[kuzushiji_r1000$Label]
#' )
#' }
#' @references
#' "KMNIST Dataset" (created by CODH), adapted from "Kuzushiji Dataset" (created by NIJL and others),
#' doi:10.20676/00000341
#' <https://github.com/rois-codh/kmnist>
#'
#' Clanuwat, T., Bober-Irizar, M., Kitamoto, A., Lamb, A., Yamamoto, K., & Ha, D. (2018).
#' Deep Learning for Classical Japanese Literature.
#' *arXiv preprint* *arXiv:1812.01718*.
#'
download_kuzushiji_mnist <- function(
  base_url = kuzushiji_mnist_url,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
) {
  as <- image_result_as(as)
  res <- download_mnist_dataset(
    base_url = base_url,
    verbose = verbose,
    as = "list",
    timeout = timeout,
    dataset = "Kuzushiji-MNIST"
  )
  if (as == "list") return(res)
  data.frame(res$data, Label = res$meta$label)
}
