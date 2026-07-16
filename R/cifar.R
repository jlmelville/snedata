#' Download CIFAR-10
#'
#' Download CIFAR-10 database of images.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame or a matrix/list result.
#'
#' The CIFAR-10 dataset contains 60000 32 x 32 color images, divided into ten
#' different classes, with 6000 images per class.
#'
#' @format A data frame with 3074 variables:
#'
#' * `r1`, `r2`, `r3` ... `r1024`: Integer pixel value
#'   of the red channel of the image, from 0 to 255.
#' * `g1`, `g2`, `g3` ... `g1024`: Integer pixel value
#'   of the green channel of the image, from 0 to 255.
#' * `b1`, `b2`, `b3` ... `b1024`: Integer pixel value
#'   of the blue channel of the image, from 0 to 255.
#' * `Label`: The image category, represented by a factor in the
#'   range 0-9.
#' * `Description`: The name of the image category associated with
#'   `Label`, represented by a factor.
#'
#' The pixel features are organized row-wise from the top left of each image.
#' The `Label` levels correspond to the following class names (stored in
#' the `Description` column):
#' * `0`: Airplane
#' * `1`: Automobile
#' * `2`: Bird
#' * `3`: Cat
#' * `4`: Deer
#' * `5`: Dog
#' * `6`: Frog
#' * `7`: Horse
#' * `8`: Ship
#' * `9`: Truck
#'
#' There are 60,000 items in the data set. The first 50,000 are the training
#' set, and the remaining 10,000 are the testing set.
#'
#' Items in the dataset can be visualized with the
#' [show_cifar()] function.
#'
#' For more information see
#' <https://www.cs.toronto.edu/~kriz/cifar.html>.
#'
#' @param url URL of the CIFAR-10 data.
#' @param destfile Filename for where to download the CIFAR-10 tarfile. If
#'   `NULL`, a file in a temporary work directory is used. The archive is
#'   always extracted to a separate temporary work directory.
#' @param cleanup If `TRUE`, then `destfile` and the untarred data
#'  will be deleted before the function returns. Only worth setting to
#'  `FALSE` to debug problems.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"matrix"` for a list with `data`, `labels`, and
#'   `descriptions`.
#' @return If `as = "data.frame"`, a data frame containing the CIFAR-10
#'   dataset. If `as = "matrix"`, a list with `data`, an integer
#'   matrix with one image per row, `labels`, a factor of numeric class
#'   labels, and `descriptions`, a factor of class names.
#' @export
#' @examples
#' \dontrun{
#' # download the data set
#' cifar10 <- download_cifar10(verbose = TRUE)
#'
#' # first 50,000 instances are the training set
#' cifar10_train <- head(cifar10, 50000)
#' # the remaining 10,000 are the test set
#' cifar10_test <- tail(cifar10, 10000)
#'
#' # PCA on 1000 examples
#' cifar10_r1000 <- cifar10[sample(nrow(cifar10), 1000), ]
#' pca <- prcomp(cifar10_r1000[, 1:(32 * 32)], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = cifar10_r1000$Label,
#'   col = rainbow(length(levels(cifar10$Label)))[cifar10_r1000$Label]
#' )
#' }
#' @references
#' The CIFAR-10 dataset
#' <https://www.cs.toronto.edu/~kriz/cifar.html>
#'
#' Krizhevsky, A., & Hinton, G. (2009).
#' *Learning multiple layers of features from tiny images*
#' (Vol. 1, No. 4, p. 7).
#' Technical report, University of Toronto.
#' @export
download_cifar10 <- function(
  url = "https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz",
  destfile = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  if (is.null(destfile)) {
    workdir <- tempfile("cifar10-")
    dir.create(workdir)
    destfile <- file.path(workdir, basename(url))
    owned_paths <- workdir
  } else {
    if (!endsWith(destfile, "tar.gz")) {
      destfile <- paste0(destfile, ".tar.gz")
    }
    parent <- dirname(destfile)
    if (!dir.exists(parent)) {
      stop("Directory does not exist: ", parent, call. = FALSE)
    }
    workdir <- tempfile("cifar10-", tmpdir = parent)
    dir.create(workdir)
    owned_paths <- c(destfile, workdir)
  }
  if (cleanup) {
    on.exit(cleanup_owned_paths(owned_paths, verbose = verbose), add = TRUE)
  }

  exdir <- file.path(workdir, "extracted")
  dir.create(exdir)

  download_asset(url, destfile, verbose = verbose)
  extract_tar_safely(
    destfile,
    exdir,
    asset = "CIFAR-10 archive",
    validate_layout = validate_cifar_archive_layout
  )
  extracted_dir <- file.path(exdir, "cifar-10-batches-bin")

  res <- matrix(nrow = 3072, ncol = 60000)
  labels <- rep(0, 60000)

  for (i in 1:6) {
    if (i != 6) {
      file <- paste0("data_batch_", i, ".bin")
    } else {
      file <- paste0("test_batch.bin")
    }
    path <- file.path(extracted_dir, file)

    batch_res <- read_cifar_bin(path, verbose = verbose)
    batch_range <- ((i - 1) * 10000 + 1):(i * 10000)
    res[, batch_range] <- batch_res$images
    labels[batch_range] <- batch_res$labels
  }

  res <- t(res)
  format_cifar_result(res, labels, as = as)
}

validate_cifar_archive_layout <- function(entries, asset) {
  root <- "cifar-10-batches-bin"
  expected_files <- c(
    paste0(root, "/data_batch_", 1:5, ".bin"),
    paste0(root, "/test_batch.bin"),
    paste0(root, "/batches.meta.txt"),
    paste0(root, "/readme.html")
  )
  missing <- setdiff(expected_files, entries)
  unexpected <- setdiff(entries, c(root, expected_files))

  if (length(missing) > 0L || length(unexpected) > 0L) {
    stop(
      asset,
      " has an invalid layout: missing ",
      length(missing),
      " and unexpected ",
      length(unexpected),
      " entries",
      call. = FALSE
    )
  }
}

cifar_description_levels <- function() {
  c(
    "airplane",
    "automobile",
    "bird",
    "cat",
    "deer",
    "dog",
    "frog",
    "horse",
    "ship",
    "truck"
  )
}

cifar_pixel_names <- function(n_pixels = 32 * 32 * 3) {
  if (n_pixels %% 3 != 0) {
    stop("CIFAR pixel matrix must have a column count divisible by 3")
  }
  n_channel_pixels <- n_pixels / 3
  c(
    paste0("r", seq_len(n_channel_pixels)),
    paste0("g", seq_len(n_channel_pixels)),
    paste0("b", seq_len(n_channel_pixels))
  )
}

format_cifar_result <- function(
  images,
  labels,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  label_values <- if (is.factor(labels)) {
    as.integer(as.character(labels))
  } else {
    as.integer(labels)
  }
  label_factor <- factor(label_values, levels = 0:9)
  description_levels <- cifar_description_levels()
  descriptions <- factor(
    description_levels[label_values + 1L],
    levels = description_levels
  )

  if (nrow(images) != length(label_factor)) {
    stop(
      "Image row count (",
      nrow(images),
      ") does not match label count (",
      length(label_factor),
      ")"
    )
  }

  colnames(images) <- cifar_pixel_names(ncol(images))

  if (as == "matrix") {
    return(list(
      data = images,
      labels = label_factor,
      descriptions = descriptions
    ))
  }

  data.frame(images, Label = label_factor, Description = descriptions)
}

#' Visualize CIFAR-10 image.
#'
#' Display a CIFAR-10 image.
#'
#' @param df Data frame containing the CIFAR-10 dataframe.
#' @param n Row index of the image to display.
#' @param interpolate If `TRUE`, use linear interpolation to smooth the
#'   image. This can help when trying to confirm that you really are looking at
#'   a tiny image of a frog.
#' @examples
#' \dontrun{
#' # show the image at position 27001 (it's a plane)
#' show_cifar(cifar10, 27001)
#' # bit easier to see it's a plane
#' show_cifar(cifar10, 27001, interpolate = TRUE)
#' }
#' @export
show_cifar <- function(df, n, interpolate = FALSE) {
  show_cifarv(as.numeric(df[n, 1:3072]), interpolate = interpolate)
}

read_cifar_bin <- function(file, verbose = FALSE) {
  if (verbose) {
    message("Reading ", file)
  }
  f <- base::file(file, "rb")
  on.exit(close(f), add = TRUE)

  n_images <- 10000
  n_pixels <- 3072
  record_size <- n_pixels + 1
  expected_bytes <- n_images * record_size

  batch <- readBin(
    f,
    what = "integer",
    n = expected_bytes,
    size = 1,
    signed = FALSE,
    endian = "little"
  )
  if (length(batch) != expected_bytes) {
    stop(
      "Expected ",
      expected_bytes,
      " bytes in CIFAR batch but read ",
      length(batch)
    )
  }

  extra <- readBin(
    f,
    what = "integer",
    n = 1,
    size = 1,
    signed = FALSE,
    endian = "little"
  )
  if (length(extra) != 0) {
    stop("CIFAR batch contains more than ", expected_bytes, " bytes")
  }

  records <- matrix(batch, nrow = record_size)
  labels <- records[1, ]
  images <- records[-1, , drop = FALSE]

  list(
    images = images,
    labels = labels
  )
}

show_cifarv <- function(
  v,
  x1 = 100,
  x2 = 250,
  y1 = 300,
  y2 = 450,
  interpolate = FALSE
) {
  r <- v[1:1024]
  g <- v[1025:2048]
  b <- v[2049:3072]

  img <- grDevices::rgb(r, g, b, maxColorValue = 255)
  dim(img) <- c(32, 32)

  graphics::plot(
    c(x1, x2),
    c(y1, y2),
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  graphics::rasterImage(t(img), x1, y1, x2, y2, interpolate = interpolate)
}
