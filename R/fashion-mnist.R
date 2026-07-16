# Base URL of the Fashion MNIST dataset website
fashion_mnist_url <-
  "http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/"

#' Download Fashion-MNIST
#'
#' Download Fashion-MNIST database of images of fashion products.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame or canonical image result. The dataset is intended to be a drop-in
#' replacement for the MNIST digits dataset but with more relevance for
#' benchmarking machine learning algorithms (i.e. it's more difficult).
#'
#' @format A data frame with 786 variables:
#'
#' * `px1`, `px2`, `px3` ... `px784`: Integer pixel
#'   value, from 0 (white) to 255 (black).
#' * `Label`: The fashion item represented by the image, in the range
#'   0-9.
#' * `Description`: The name of the fashion item associated with the
#'   `Label`
#'
#' Pixels are organized row-wise. The `Label` variable is stored as a
#' factor. The labels correspond to:
#'
#' * `0`: T-shirt/top
#' * `1`: Trouser
#' * `2`: Pullover
#' * `3`: Dress
#' * `4`: Coat
#' * `5`: Sandal
#' * `6`: Shirt
#' * `7`: Sneaker
#' * `8`: Bag
#' * `9`: Ankle boot
#'
#' and are also present as the `Description` factor.
#'
#' There are 70,000 items in the data set. The first 60,000 are the training
#' set, as found in the `train-images-idx3-ubyte.gz` file. The remaining
#' 10,000 are the test set, from the `t10k-images-idx3-ubyte.gz` file.
#'
#' Items in the dataset can be visualized with the
#' [show_mnist_digit()] function.
#'
#' For more information see
#' <https://github.com/zalandoresearch/fashion-mnist>.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"list"` for the canonical image result described in
#'   [download_mnist()].
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return A data frame containing Fashion-MNIST, or a canonical image result
#'   with `label` and `description` factors in `meta`.
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
#' *arXiv preprint* *arXiv:1708.07747*.
#' <https://github.com/zalandoresearch/fashion-mnist/>
#' @export
download_fashion_mnist <- function(
  base_url = fashion_mnist_url,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
) {
  as <- image_result_as(as)
  res <- download_mnist(
    base_url = base_url,
    verbose = verbose,
    as = "list",
    timeout = timeout
  )

  description_levels <- c(
    "T-shirt/top",
    "Trouser",
    "Pullover",
    "Dress",
    "Coat",
    "Sandal",
    "Shirt",
    "Sneaker",
    "Bag",
    "Ankle boot"
  )
  res$meta$description <- factor(
    description_levels[as.integer(as.character(res$meta$label)) + 1L],
    levels = description_levels
  )
  res$source$dataset <- "Fashion-MNIST"
  if (as == "list") return(res)
  data.frame(
    res$data,
    Label = res$meta$label,
    Description = res$meta$description
  )
}
