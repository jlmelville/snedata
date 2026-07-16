#' Visualize MNIST digit.
#'
#' Display an MNIST digit as an image the hand-written digit represented by the
#' nth row in a data frame.
#'
#' @note Originally based on a function by Brendan O'Connor, which can be found
#' at <https://gist.github.com/brendano/39760>.
#' @param df Data frame containing MNIST digits.
#' @param n Row index of the digit to display.
#' @param col List of colors to use in the display.
#' @param ... Other arguments passed onto the [graphics::image()]
#' function.
#' @examples
#' \dontrun{
#' # show the fifth digit
#' mnist <- download_mnist()
#' show_mnist_digit(mnist, 5)
#' }
#' @export
show_mnist_digit <- function(df, n, col = grDevices::gray(1:255 / 255), ...) {
  data <- image_result_data(df)
  graphics::image(
    matrix(as.numeric(data[n, 1:784]), nrow = 28)[, 28:1],
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
#' the <https://github.com/fgnt/mnist> mirror of the original MNIST files
#' and converts them to a data frame or canonical list result.
#'
#' @format A data frame with 785 variables:
#'
#' * `px1`, `px2`, `px3` ... `px784`: Integer pixel
#'   value, from 0 (white) to 255 (black).
#' * `Label`: The digit represented by the image, in the range 0-9.
#'
#' Pixels are organized row-wise. The `Label` variable is stored as a
#' factor.
#'
#' There are 70,000 digits in the data set. The first 60,000 are the training
#' set, as found in the `train-images-idx3-ubyte.gz` file. The remaining
#' 10,000 are the test set, from the `t10k-images-idx3-ubyte.gz` file.
#'
#' Items in the dataset can be visualized with
#' [show_mnist_digit()].
#'
#' For more information about the original dataset see
#' <http://yann.lecun.com/exdb/mnist>.
#'
#' @param base_url Base URL that the MNIST files are located at.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"list"` for the canonical image result.
#' @section Canonical list results:
#' `as = "list"` returns a shallow list with `data` (one image per row),
#' `meta` (one metadata row per image), `image_dim`, `channel_order`, and
#' `source`. Metadata uses lower-case invariant names when applicable: `label`,
#' `description`, `split`, `id`, `object`, and `pose`; dataset-specific fields
#' are retained in `meta`. `split` is explicit, so train/test identity does not
#' depend on row position. `source` records the dataset and acquisition URL.
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return If `as = "data.frame"`, a data frame containing the MNIST
#'   digits. If `as = "list"`, a canonical image result with an integer
#'   matrix in `data` and factor digit labels in `meta$label`.
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
  as = c("data.frame", "list"),
  timeout = 1800
) {
  with_download_timeout(
    {
      as <- image_result_as(as)
      train <- parse_files(
        "train-images-idx3-ubyte.gz",
        "train-labels-idx1-ubyte.gz",
        base_url = base_url,
        verbose = verbose,
        split = "training",
        dataset = "MNIST"
      )
      test <- parse_files(
        "t10k-images-idx3-ubyte.gz",
        "t10k-labels-idx1-ubyte.gz",
        base_url = base_url,
        verbose = verbose,
        split = "testing",
        dataset = "MNIST"
      )
      combine_image_label_results(train, test, as = as)
    },
    timeout = timeout
  )
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
  parse_mnist_image_connection(f, asset = filename)
}

parse_mnist_image_connection <- function(f, asset) {
  asset <- paste0("MNIST image asset '", asset, "'")
  magic <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "magic number",
    endian = "big"
  )
  validate_binary_magic(magic, expected = 2051L, asset = asset)

  n_images <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "image count",
    endian = "big",
    header = c(magic = magic)
  )
  n_rows <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "row count",
    endian = "big",
    header = c(magic = magic, n_images = n_images)
  )
  n_cols <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "column count",
    endian = "big",
    header = c(magic = magic, n_images = n_images, n_rows = n_rows)
  )
  dimensions <- c(n_images = n_images, n_rows = n_rows, n_cols = n_cols)
  pixel_count <- binary_safe_product(dimensions, asset)
  pixels_per_image <- binary_safe_product(
    dimensions[c("n_rows", "n_cols")],
    asset,
    header = dimensions
  )
  pixels <- read_binary_exact(
    f,
    what = "integer",
    n = pixel_count,
    size = 1L,
    signed = FALSE,
    asset = asset,
    component = "image payload",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  images <- matrix(pixels, ncol = pixels_per_image, byrow = TRUE)
  attr(images, "image_dim") <- c(height = n_rows, width = n_cols)
  images
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
  parse_mnist_label_connection(f, asset = filename)
}

parse_mnist_label_connection <- function(f, asset) {
  asset <- paste0("MNIST label asset '", asset, "'")
  magic <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "magic number",
    endian = "big"
  )
  validate_binary_magic(magic, expected = 2049L, asset = asset)

  n_labels <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "label count",
    endian = "big",
    header = c(magic = magic)
  )
  dimensions <- c(n_labels = n_labels)
  label_count <- binary_safe_product(dimensions, asset)
  labels <- read_binary_exact(
    f,
    what = "integer",
    n = label_count,
    size = 1L,
    signed = FALSE,
    asset = asset,
    component = "label payload",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  labels
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
# @param split Dataset split represented by the files.
# @param dataset Dataset name recorded in the canonical result source.
# @return Canonical image result containing images and labels.
parse_files <- function(
  image_filename,
  label_filename,
  base_url = mnist_url,
  label_parser = parse_label_file,
  verbose = FALSE,
  split = "training",
  dataset = "MNIST"
) {
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

  if (nrow(images) != length(labels)) {
    stop(
      "MNIST image/label count mismatch: image asset '",
      image_filename,
      "' has expected count ",
      nrow(images),
      " labels from header dimensions n_images=",
      nrow(images),
      ", pixels_per_image=",
      ncol(images),
      "; label asset '",
      label_filename,
      "' has actual count ",
      length(labels),
      " labels; expected matching image and label counts",
      call. = FALSE
    )
  }

  format_image_label_result(
    images,
    labels,
    split = split,
    source = list(dataset = dataset, url = base_url)
  )
}

format_image_label_result <- function(
  images,
  labels,
  split = "training",
  source = list(dataset = "MNIST", url = mnist_url)
) {
  qmnist_metadata <- attr(labels, "qmnist_metadata")
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
  meta <- data.frame(
    id = seq_len(nrow(images)),
    split = factor(rep(split, nrow(images)), levels = c("training", "testing")),
    label = labels
  )
  if (identical(source$dataset, "QMNIST")) {
    if (is.null(qmnist_metadata)) {
      qmnist_metadata <- matrix(
        NA_integer_,
        nrow = nrow(images),
        ncol = length(qmnist_extended_label_columns),
        dimnames = list(NULL, qmnist_extended_label_columns)
      )
    }
    meta <- cbind(meta, as.data.frame(qmnist_metadata))
  }
  image_dim <- attr(images, "image_dim")
  if (is.null(image_dim)) {
    side <- sqrt(ncol(images))
    image_dim <- c(height = side, width = side)
  }
  new_image_result(
    images,
    meta = meta,
    image_dim = image_dim,
    channel_order = "gray",
    source = source
  )
}

combine_image_label_results <- function(
  train,
  test,
  as = c("data.frame", "list")
) {
  as <- image_result_as(as)
  result <- new_image_result(
    data = rbind(train$data, test$data),
    meta = rbind(train$meta, test$meta),
    image_dim = train$image_dim,
    channel_order = train$channel_order,
    source = train$source
  )
  result$meta$id <- seq_len(nrow(result$data))
  if (as == "list") return(result)
  data.frame(result$data, Label = result$meta$label)
}
