#' Visualize MNIST digit.
#'
#' Display an MNIST digit as an image the hand-written digit represented by the
#' nth row in a data frame.
#'
#' @note Originally based on a function by Brendan O'Connor, which can be found
#' at \url{https://gist.github.com/brendano/39760}.
#' @param df Data frame containing MNIST digits.
#' @param n Row index of the digit to display.
#' @param col List of colors to use in the display.
#' @param ... Other arguments passed onto the \code{\link[graphics]{image}}
#' function.
#' @examples
#' \dontrun{
#' # show the fifth digit
#' mnist <- download_mnist()
#' show_mnist_digit(mnist, 5)
#' }
#' @export
show_mnist_digit <- function(df, n, col = grDevices::gray(1:255 / 255), ...) {
  graphics::image(
    matrix(as.numeric(df[n, 1:784]), nrow = 28)[, 28:1],
    col = col,
    ...
  )
}

# Base URL of the MNIST digits dataset mirror
mnist_url <- "https://github.com/fgnt/mnist/raw/refs/heads/master/"

#' Download MNIST
#'
#' Download MNIST database of handwritten digits.
#'
#' Downloads the image and label files for the training and test datasets from
#' the \url{https://github.com/fgnt/mnist} mirror of the original MNIST files
#' and converts them to a data frame or a matrix/list result.
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
#' There are 70,000 digits in the data set. The first 60,000 are the training
#' set, as found in the \code{train-images-idx3-ubyte.gz} file. The remaining
#' 10,000 are the test set, from the \code{t10k-images-idx3-ubyte.gz} file.
#'
#' Items in the dataset can be visualized with
#' \code{\link{show_mnist_digit}}.
#'
#' For more information about the original dataset see
#' \url{http://yann.lecun.com/exdb/mnist}.
#'
#' @param base_url Base URL that the MNIST files are located at.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @param as Return format. Use \code{"data.frame"} for the original data frame
#'   shape, or \code{"matrix"} for a list with \code{data} and \code{labels}.
#' @return If \code{as = "data.frame"}, a data frame containing the MNIST
#'   digits. If \code{as = "matrix"}, a list with \code{data}, an integer matrix
#'   with one image per row, and \code{labels}, a factor of digit labels.
#' @note Originally based on a function by Brendan O'Connor.
#' @export
#' @examples
#' \dontrun{
#' # download the MNIST data set
#' mnist <- download_mnist()
#'
#' # first 60,000 instances are the training set
#' mnist_train <- head(mnist, 60000)
#' # the remaining 10,000 are the test set
#' mnist_test <- tail(mnist, 10000)
#'
#' # PCA on 1000 random training examples
#' mnist_r1000 <- mnist_train[sample(nrow(mnist_train), 1000), ]
#' pca <- prcomp(mnist_r1000[, 1:784], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = mnist_r1000$Label,
#'   col = rainbow(length(levels(mnist$Label)))[mnist_r1000$Label]
#' )
#' }
#' @export
download_mnist <- function(
  base_url = mnist_url,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  train <- parse_files(
    "train-images-idx3-ubyte.gz",
    "train-labels-idx1-ubyte.gz",
    base_url = base_url,
    verbose = verbose,
    as = as
  )
  test <- parse_files(
    "t10k-images-idx3-ubyte.gz",
    "t10k-labels-idx1-ubyte.gz",
    base_url = base_url,
    verbose = verbose,
    as = as
  )
  combine_image_label_results(train, test, as = as)
}

# Open Gzipped Binary File at URL
#
# Opens a file at a specified URL and returns the connection for further
# processing. Callers must close the connection when they're done.
#
# @param filename Name of the file to open.
# @param base_url URL of the resource containing the file.
# @param verbose If \code{TRUE}, generate a diagnostic message when the file
# is opened.
# @return Opened connection to the file.
open_binary_file <- function(filename, base_url = mnist_url, verbose = FALSE) {
  conn <- paste0(base_url, filename)
  if (verbose) {
    message("Downloading ", conn)
  }
  gzcon(url(conn, "rb"))
}

# Parse Image File
#
# Downloads a gzipped MNIST image file.
#
# @param filename The image filename.
# @param base_url URL of the resource containing the \code{filename}.
# @param verbose If \code{TRUE}, generate a diagnostic message as files are
# downloaded.
# @return Vector of integers representing the digits.
parse_image_file <- function(filename, base_url = mnist_url, verbose = FALSE) {
  f <- open_binary_file(filename, base_url = base_url, verbose = verbose)
  on.exit(close(f), add = TRUE)
  magic <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  if (magic != 2051) {
    stop(
      "First four bytes of image file should be magic number 2051 but was ",
      magic
    )
  }
  n <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  nrow <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  ncol <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  x <- readBin(f, "integer", n = n * nrow * ncol, size = 1, signed = FALSE)
  matrix(x, ncol = nrow * ncol, byrow = TRUE)
}

# Parse Label File
#
# Downloads a gzipped MNIST label file.
#
# @param filename The label filename.
# @param base_url URL of the resource containing \code{filename}.
# @param verbose If \code{TRUE}, generate a diagnostic message as files are
# downloaded.
# @return Vector of integers representing the digits.
parse_label_file <- function(filename, base_url = mnist_url, verbose = FALSE) {
  f <- open_binary_file(filename, base_url = base_url, verbose = verbose)
  on.exit(close(f), add = TRUE)
  magic <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  if (magic != 2049) {
    stop(
      "First four bytes of label file should be magic number 2049 but was ",
      magic
    )
  }
  n <- readBin(f, "integer", n = 1, size = 4, endian = "big")
  y <- readBin(f, "integer", n = n, size = 1, signed = FALSE)
  y
}

# Parse Image and Label File Pair
#
# Downloads an image file and a corresponding label file, combining
# them into a data frame.
#
# @param image_filename The image filename.
# @param label_filename The label filename corresponding to the images in
#   \code{image_filename}.
# @param base_url URL of the resource containing the files.
# @param label_parser Function to parse label files. Default is
#  parse_label_file, which works with standard idx1-ubyte files used with most
#  MNIST-like projects.
# @param verbose If \code{TRUE}, generate a diagnostic message as files are
#   downloaded.
# @param as Return format. See \code{\link{download_mnist}}.
# @return Data frame or matrix/list result containing images and labels.
parse_files <- function(
  image_filename,
  label_filename,
  base_url = mnist_url,
  label_parser = parse_label_file,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  images <- parse_image_file(
    image_filename,
    base_url = base_url,
    verbose = verbose
  )
  labels <- label_parser(
    label_filename,
    base_url = base_url,
    verbose = verbose
  )

  format_image_label_result(images, labels, as = as)
}

format_image_label_result <- function(
  images,
  labels,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  labels <- factor(labels)

  if (nrow(images) != length(labels)) {
    stop(
      "Image row count (",
      nrow(images),
      ") does not match label count (",
      length(labels),
      ")"
    )
  }

  colnames(images) <- paste0("px", seq_len(ncol(images)))

  if (as == "matrix") {
    return(list(data = images, labels = labels))
  }

  df <- as.data.frame(images)
  df$Label <- labels
  df
}

combine_image_label_results <- function(
  train,
  test,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  if (as == "matrix") {
    return(list(
      data = rbind(train$data, test$data),
      labels = c(train$labels, test$labels)
    ))
  }

  rbind(train, test)
}
