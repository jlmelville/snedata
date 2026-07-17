qmnist_url <- "https://github.com/facebookresearch/qmnist/raw/refs/heads/main/"

#' Download QMNIST
#'
#' Download QMNIST database of handwritten digits. This is the same as the
#' MNIST digits, but with an extended test dataset of 60,000 digits.
#'
#' Downloads the image and label files for the training and test datasets from
#' <https://github.com/facebookresearch/qmnist> and converts them to a data
#' frame or canonical list result.
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
#' There are 120,000 digits in the data set. The first 60,000 are the training
#' set, as found in the `qmnist-train-images-idx3-ubyte.gz` file. The
#' remaining 60,000 are the test set, from the
#' `qmnist-test-images-idx3-ubyte.gz` file.
#'
#' Items in the dataset can be visualized with
#' [show_mnist_digit()].
#'
#' For more information see <https://github.com/facebookresearch/qmnist>.
#'
#' @param base_url Base URL that the QMNIST files are located at.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"list"` for the canonical image result described in
#'   [download_mnist()].
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return If `as = "data.frame"`, a data frame containing the QMNIST
#'   digits. If `as = "list"`, a canonical image result. Its `meta` includes
#'   all eight QMNIST extended-label fields in addition to `id`, `split`, and
#'   `label`.
#' @export
#' @examples
#' \dontrun{
#' # download the QMNIST data set
#' qmnist <- download_qmnist()
#'
#' # first 60,000 instances are the training set
#' qmnist_train <- head(qmnist, 60000)
#' # the remaining 60,000 are the test set
#' qmnist_test <- tail(qmnist, 60000)
#' }
#' @references
#' Yadav, C., & Bottou, L. (2019, May).
#' Cold Case: The Lost MNIST Digits
#' *arXiv preprint* *arXiv:1905.10498*.
#' <https://github.com/facebookresearch/qmnist>
download_qmnist <- function(
  base_url = qmnist_url,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
) {
  with_download_timeout(
    {
      as <- image_result_as(as)
      train <- parse_files(
        "qmnist-train-images-idx3-ubyte.gz",
        "qmnist-train-labels-idx2-int.gz",
        label_parser = parse_extended_label_file,
        base_url = base_url,
        verbose = verbose,
        split = "training",
        dataset = "QMNIST"
      )
      test <- parse_files(
        "qmnist-test-images-idx3-ubyte.gz",
        "qmnist-test-labels-idx1-ubyte.gz",
        base_url = base_url,
        verbose = verbose,
        split = "testing",
        dataset = "QMNIST"
      )
      if (verbose) {
        n_train <- nrow(train$data)
        n_test <- nrow(test$data)
        message("Read ", n_train, " training and ", n_test, " images")
      }
      combine_image_label_results(train, test, as = as)
    },
    timeout = timeout
  )
}

# Several labels are available for QMNIST
qmnist_extended_label_columns <- c(
  "character_class",
  "nist_hsf_series",
  "nist_writer_id",
  "digit_index",
  "nist_class_code",
  "nist_global_digit_index",
  "duplicate",
  "unused"
)

parse_extended_label_file <- function(
  filename,
  base_url = qmnist_url,
  verbose = FALSE
) {
  f <- open_binary_file(filename, base_url = base_url, verbose = verbose)
  on.exit(close(f), add = TRUE)
  parse_qmnist_extended_label_connection(f, asset = filename)
}

parse_qmnist_extended_label_connection <- function(f, asset) {
  asset <- paste0("QMNIST extended label asset '", asset, "'")
  magic <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "magic number",
    endian = "big"
  )
  validate_binary_magic(magic, expected = 3074L, asset = asset)

  n_rows <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "row count",
    endian = "big",
    header = c(magic = magic)
  )
  n_cols <- read_binary_scalar(
    f,
    what = "integer",
    size = 4L,
    asset = asset,
    component = "column count",
    endian = "big",
    header = c(magic = magic, n_rows = n_rows)
  )
  dimensions <- validate_binary_dimensions(
    c(n_rows = n_rows, n_cols = n_cols),
    asset
  )

  if (dimensions[["n_cols"]] != length(qmnist_extended_label_columns)) {
    stop(
      asset,
      " has an invalid column count: expected count ",
      length(qmnist_extended_label_columns),
      " columns (",
      length(qmnist_extended_label_columns) * 4L,
      " bytes per row); actual count ",
      dimensions[["n_cols"]],
      " columns; header dimensions: ",
      binary_header_context(dimensions),
      call. = FALSE
    )
  }

  value_count <- binary_safe_product(dimensions, asset)
  values <- read_binary_exact(
    f,
    what = "integer",
    n = value_count,
    size = 4L,
    asset = asset,
    component = "extended label payload",
    endian = "big",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  metadata <- matrix(values, ncol = dimensions[["n_cols"]], byrow = TRUE)
  colnames(metadata) <- qmnist_extended_label_columns
  labels <- metadata[, "character_class"]
  attr(labels, "qmnist_metadata") <- metadata
  labels
}
