write_image_result_mnist_images <- function(path, pixels) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)
  writeBin(as.integer(c(2051L, 2L, 2L, 2L)), f, size = 4L, endian = "big")
  writeBin(as.integer(pixels), f, size = 1L)
}

write_image_result_mnist_labels <- function(path, labels) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)
  writeBin(as.integer(c(2049L, length(labels))), f, size = 4L, endian = "big")
  writeBin(as.integer(labels), f, size = 1L)
}

image_result_file_base_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE), "/")
}

test_that("canonical image results share metadata, dimensions, and row order", {
  mnist <- format_image_label_result(
    structure(
      matrix(1:8, nrow = 2, byrow = TRUE),
      image_dim = c(height = 2L, width = 2L)
    ),
    c(1L, 9L),
    split = "training",
    source = list(dataset = "MNIST", url = "local")
  )
  cifar <- format_cifar_result(
    matrix(rep(1:255, length.out = 2L * 3072L), nrow = 2),
    c(0L, 9L),
    split = c("training", "testing"),
    source = list(dataset = "CIFAR-10", url = "local"),
    as = "list"
  )
  norb <- format_norb_result(
    matrix(rep(1:255, length.out = 2L * 96L * 96L * 2L), nrow = 2),
    matrix(c(1, 2, 4, 5, 5, 6, 8, 0), nrow = 4),
    c(0L, 4L),
    split = "testing",
    as = "list",
    source = list(dataset = "Small NORB", url = "local")
  )
  coil <- format_coil_result(
    matrix(1:4, nrow = 1),
    objects = 2L,
    poses = 10L,
    as = "list",
    source = list(dataset = "COIL", url = "local")
  )

  for (result in list(mnist, cifar, norb, coil)) {
    expect_named(
      result,
      c("data", "meta", "image_dim", "channel_order", "source")
    )
    expect_equal(nrow(result$data), nrow(result$meta))
    expect_true(all(c("id", "label") %in% names(result$meta)))
    expect_true(all(result$image_dim > 0L))
    expect_named(result$source, c("dataset", "url"))
  }

  expect_equal(unname(mnist$data[2, ]), 5:8)
  expect_equal(levels(mnist$meta$label), c("1", "9"))
  expect_equal(as.character(cifar$meta$split), c("training", "testing"))
  expect_equal(cifar$channel_order, c("red", "green", "blue"))
  expect_equal(
    names(norb$meta),
    c(
      "id",
      "instance",
      "elevation",
      "azimuth",
      "lighting",
      "split",
      "label",
      "description"
    )
  )
  expect_equal(as.character(norb$meta$description), c("Animal", "Car"))
  expect_equal(coil$meta$id, "2_10")
  expect_equal(coil$meta$pose, 10L)
})

test_that("new_image_result rejects inconsistent canonical objects", {
  make_result <- function(
    data = matrix(1:4, nrow = 1),
    meta = data.frame(id = 1L),
    image_dim = c(height = 2L, width = 2L),
    channel_order = "gray",
    source = list(dataset = "fixture", url = "local")
  ) {
    new_image_result(data, meta, image_dim, channel_order, source)
  }

  expect_error(
    make_result(data = matrix(letters[1:4], nrow = 1)),
    "numeric matrix"
  )
  expect_error(
    make_result(meta = data.frame(id = 1:2)),
    "one row per image"
  )
  expect_error(
    make_result(image_dim = c(height = 1.5, width = 2)),
    "positive integer image dimensions"
  )
  expect_error(
    make_result(image_dim = c(height = 2L, height = 2L)),
    "unique, nonempty dimension names"
  )
  expect_error(
    make_result(image_dim = stats::setNames(c(2L, 2L), c("", "width"))),
    "unique, nonempty dimension names"
  )
  expect_error(
    make_result(image_dim = c(height = 1L, width = 3L)),
    "describes 3 pixels but `data` has 4 columns"
  )
  expect_error(
    make_result(image_dim = c(height = .Machine$integer.max, width = 2L)),
    "more than 2147483647 matrix columns"
  )
  expect_error(
    make_result(
      image_dim = c(height = 1L, width = 2L, channels = 2L),
      channel_order = "gray"
    ),
    "has 1 names but `image_dim` declares 2"
  )
  expect_error(
    make_result(channel_order = c("left", "right")),
    "has 2 names but `image_dim` declares 1"
  )
  expect_error(
    make_result(
      image_dim = c(height = 1L, width = 2L, channels = 2L),
      channel_order = c("red", "red")
    ),
    "unique, nonempty channel names"
  )
  expect_error(
    make_result(
      image_dim = c(height = 1L, width = 1L, channels = 2L, cameras = 2L),
      channel_order = c("left", "right")
    ),
    "cannot declare both `channels` and `cameras`"
  )
  expect_error(
    make_result(source = "local"),
    "source.*must be a list"
  )
  expect_error(
    make_result(source = list(dataset = "", url = "local")),
    "source\\$dataset.*nonempty character scalar"
  )
  expect_error(
    make_result(source = list(dataset = "fixture", url = c("one", "two"))),
    "source\\$url.*nonempty character scalar"
  )
})

test_that("image-result combination rejects incompatible splits", {
  make_split <- function(split, dataset = "MNIST") {
    format_image_label_result(
      structure(
        matrix(1:4, nrow = 1),
        image_dim = c(height = 2L, width = 2L)
      ),
      1L,
      split = split,
      source = list(dataset = dataset, url = "local")
    )
  }
  train <- make_split("training")
  test <- make_split("testing")

  expect_silent(combine_image_label_results(train, test, as = "list"))

  bad_columns <- test
  bad_columns$data <- matrix(1:2, nrow = 1)
  expect_error(
    combine_image_label_results(train, bad_columns),
    "image column counts must match"
  )

  bad_dimensions <- test
  bad_dimensions$image_dim <- c(height = 1L, width = 4L)
  expect_error(
    combine_image_label_results(train, bad_dimensions),
    "`image_dim` values must match"
  )

  bad_channels <- test
  bad_channels$channel_order <- "infrared"
  expect_error(
    combine_image_label_results(train, bad_channels),
    "`channel_order` values must match"
  )

  bad_source <- make_split("testing", dataset = "Other")
  expect_error(
    combine_image_label_results(train, bad_source),
    "source datasets must match"
  )
})

test_that("MNIST-family wrappers expose canonical lists from local fixtures", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_image_result_mnist_images(
    file.path(tmpdir, "train-images-idx3-ubyte.gz"),
    1:8
  )
  write_image_result_mnist_labels(
    file.path(tmpdir, "train-labels-idx1-ubyte.gz"),
    c(0L, 9L)
  )
  write_image_result_mnist_images(
    file.path(tmpdir, "t10k-images-idx3-ubyte.gz"),
    9:16
  )
  write_image_result_mnist_labels(
    file.path(tmpdir, "t10k-labels-idx1-ubyte.gz"),
    c(1L, 8L)
  )

  with_mocked_bindings(
    validate_mnist_dataset = function(...) invisible(NULL),
    {
      fashion <- download_fashion_mnist(
        image_result_file_base_url(tmpdir),
        as = "list"
      )
      kuzushiji <- download_kuzushiji_mnist(
        image_result_file_base_url(tmpdir),
        as = "list"
      )
    },
    .package = "snedata"
  )

  expect_equal(
    as.character(fashion$meta$description),
    c("T-shirt/top", "Ankle boot", "Trouser", "Bag")
  )
  expect_equal(
    as.character(kuzushiji$meta$split),
    c("training", "training", "testing", "testing")
  )
  expect_equal(kuzushiji$source$dataset, "Kuzushiji-MNIST")
  expect_error(
    download_mnist(image_result_file_base_url(tmpdir), as = "matrix"),
    "should be one of"
  )
})

test_that("image viewers dispatch over legacy data frames and canonical lists", {
  mnist_data <- matrix(seq_len(784), nrow = 1)
  mnist <- new_image_result(
    mnist_data,
    data.frame(
      id = 1L,
      split = factor("training", levels = c("training", "testing")),
      label = factor("1")
    ),
    c(height = 28L, width = 28L),
    "gray",
    list(dataset = "MNIST", url = "local")
  )
  cifar_data <- matrix(rep(0L, 3072L), nrow = 1)
  cifar <- format_cifar_result(cifar_data, 0L, as = "list")
  norb_data <- matrix(rep(0L, 96L * 96L * 2L), nrow = 1)
  norb <- format_norb_result(
    norb_data,
    matrix(c(0L, 0L, 0L, 0L), nrow = 4),
    0L,
    "training",
    as = "list"
  )
  norb_df <- format_norb_result(
    norb_data,
    matrix(c(0L, 0L, 0L, 0L), nrow = 4),
    0L,
    "training"
  )
  coil_data <- matrix(seq(0, 1, length.out = 128L * 128L), nrow = 1)
  coil <- format_coil_result(coil_data, 1L, 0L, as = "list")

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(show_mnist_digit(
    data.frame(mnist_data, Label = factor("1")),
    1
  ))
  expect_silent(show_mnist_digit(mnist, 1))
  expect_silent(show_cifar(data.frame(cifar_data, Label = factor("0")), 1))
  expect_silent(show_cifar(cifar, 1))
  expect_silent(show_norb_object(norb_df, 0, 0, 0, 0, 0))
  expect_silent(show_norb_object(norb, 0, 0, 0, 0, 0))
  expect_silent(show_coil_object(coil, 1, 0))
})
