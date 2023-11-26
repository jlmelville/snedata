qmnist_url <- "https://github.com/facebookresearch/qmnist/raw/master/"

#' Download QMNIST
#'
#' Download QMNIST database of handwritten digits. This is the same as the
#' MNIST digits, but with an extended test dataset of 60,000 digits.
#'
#' Downloads the image and label files for the training and test datasets from
#' \url{https://github.com/facebookresearch/qmnist} and converts them to a data
#' frame.
#'
#' @format A data frame with 785 variables:
#'
#' \describe{
#' \item{\code{px1}, \code{px2}, \code{px3} ... \code{px784}}{Integer pixel
#'   value, from 0 (white) to 255 (black).}
#' \item{\code{Label}}{The digit represented by the image, in the range 0-9.}
#' }
#'
#' Pixels are organized row-wise. The \code{Label} variable is stored as a
#' factor.
#'
#' There are 120,000 digits in the data set. The first 60,000 are the training
#' set, as found in the \code{qmnist-train-images-idx3-ubyte.hz} file. The
#' remaining 60,000 are the test set, from the
#' \code{qmnist-test-images-idx3-ubyte.gz} file.
#'
#' Items in the dataset can be visualized with
#' \code{\link{show_mnist_digit}}.
#'
#' For more information see \url{https://github.com/facebookresearch/qmnist}.
#'
#' @param base_url Base URL that the QMNIST files are located at.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing the QMNIST digits.
#' @export
#' @examples
#' \dontrun{
#' # download the QMNIST data set
#' qmnist <- download_qmnist()
#'
#' # first 60,000 instances are the training set
#' qmnist_train <- head(qmnist, 60000)
#' # the remaining 60,000 are the test set
#' qmnist_test <- tail(mnist, 60000)
#' }
#' @references
#' Yadav, C., & Bottou, L. (2019, May).
#' Cold Case: The Lost MNIST Digits
#' \emph{arXiv preprint} \emph{arXiv:1905.10498}.
#' \url{https://github.com/facebookresearch/qmnist}
#' @export
download_qmnist <- function(base_url = qmnist_url, verbose = FALSE) {
  train <- parse_files("qmnist-train-images-idx3-ubyte.gz",
    "qmnist-train-labels-idx2-int.gz",
    label_parser = parse_extended_label_file,
    base_url = base_url, verbose = verbose
  )
  test <- parse_files("qmnist-test-images-idx3-ubyte.gz",
    "qmnist-test-labels-idx1-ubyte.gz",
    base_url = base_url, verbose = verbose
  )
  if (verbose) {
    message("Read ", nrow(train), " training and ", nrow(test), " images")
  }
  rbind(train, test)
}

# QMNIST test labels are stored as extended labels. We only extract the digit
# class.
parse_extended_label_file <- function(filename, base_url = qmnist_url,
                                      verbose = FALSE) {
  f <- open_binary_file(filename, base_url = base_url, verbose = verbose)
  magic <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  if (magic != 3074) {
    stop(
      "First four bytes of label file should be magic number 2049 but was ",
      magic
    )
  }
  nrows <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  # should be 8 for qmnist extended label file
  ncols <- readBin(f, "integer", n = 1, size = 4, endian = "big")

  y <- rep(0, nrows)

  for (n in 1:nrows) {
    row_data <- readBin(f, "integer", n = ncols, size = 4, endian = "big")
    y[n] <- row_data[1]
  }

  close(f)
  y
}
