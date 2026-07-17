file_base_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE), "/")
}

test_that("image downloader defaults use expected public sources", {
  expect_identical(
    snedata:::fashion_mnist_url,
    "https://raw.githubusercontent.com/zalandoresearch/fashion-mnist/master/data/fashion/"
  )
  expect_identical(
    snedata:::kuzushiji_mnist_url,
    "https://codh.rois.ac.jp/kmnist/dataset/kmnist/"
  )
  expect_identical(
    snedata:::newsgroups_url,
    "http://qwone.com/~jason/20Newsgroups/20news-bydate.tar.gz"
  )
})

write_gz_mnist_images <- function(
  path,
  magic = 2051,
  n_images = 2,
  n_rows = 2,
  n_cols = 2,
  pixels = seq_len(n_images * n_rows * n_cols),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(magic), f, size = 4, endian = "big")
  writeBin(as.integer(n_images), f, size = 4, endian = "big")
  writeBin(as.integer(n_rows), f, size = 4, endian = "big")
  writeBin(as.integer(n_cols), f, size = 4, endian = "big")
  writeBin(as.integer(pixels), f, size = 1)
  writeBin(as.integer(trailing), f, size = 1)
}

write_gz_mnist_labels <- function(
  path,
  magic = 2049,
  n_labels = length(labels),
  labels = c(0, 5, 9),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(magic), f, size = 4, endian = "big")
  writeBin(as.integer(n_labels), f, size = 4, endian = "big")
  writeBin(as.integer(labels), f, size = 1)
  writeBin(as.integer(trailing), f, size = 1)
}

write_gz_mnist_header <- function(path, header) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(header), f, size = 4, endian = "big")
}

write_gz_tar_link_fixture <- function(path) {
  header <- rep(as.raw(0), 512L)
  header[1:4] <- charToRaw("link")
  header[101:107] <- charToRaw("0000777")
  header[109:115] <- charToRaw("0000000")
  header[117:123] <- charToRaw("0000000")
  header[125:135] <- charToRaw("00000000000")
  header[137:147] <- charToRaw("00000000000")
  header[149:156] <- as.raw(32)
  header[157] <- charToRaw("2")
  header[158:161] <- charToRaw("file")
  header[258:263] <- c(charToRaw("ustar"), as.raw(0))
  header[264:265] <- charToRaw("00")

  checksum <- sum(as.integer(header))
  header[149:154] <- charToRaw(sprintf("%06o", checksum))
  header[155] <- as.raw(0)
  header[156] <- as.raw(32)

  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)
  writeBin(header, f)
  writeBin(raw(1024L), f)
}

write_gz_qmnist_labels <- function(
  path,
  magic = 3074,
  metadata = rbind(
    c(7, 0, 610, 12, 37, 1001, 0, 0),
    c(4, 4, 2100, 13, 38, 1002, 0, 0)
  ),
  n_rows = nrow(metadata),
  n_cols = ncol(metadata),
  values = as.integer(t(metadata)),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(magic), f, size = 4, endian = "big")
  writeBin(as.integer(n_rows), f, size = 4, endian = "big")
  writeBin(as.integer(n_cols), f, size = 4, endian = "big")
  writeBin(as.integer(values), f, size = 4, endian = "big")
  writeBin(as.integer(trailing), f, size = 4, endian = "big")
}

write_norb_matrix_header <- function(
  f,
  type,
  ndim,
  padding = c(0, 0, 0)
) {
  magic <- switch(
    type,
    byte = c(85L, 76L, 61L, 30L),
    integer = c(84L, 76L, 61L, 30L)
  )
  writeBin(as.integer(magic), f, size = 1)
  writeBin(as.integer(c(ndim, padding)), f, size = 1)
}

write_gz_norb_images <- function(
  path,
  type = "byte",
  ndim = 4,
  header_padding = c(0, 0, 0),
  dimensions = c(2, 2, 96, 96),
  values = rep(seq_len(dimensions[[1]]), each = prod(dimensions[-1])),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, type, ndim, padding = header_padding)
  writeBin(as.integer(dimensions), f, size = 4, endian = "little")
  writeBin(as.integer(values), f, size = 1)
  writeBin(as.integer(trailing), f, size = 1)
}

write_gz_norb_categories <- function(
  path,
  type = "integer",
  ndim = 1,
  header_padding = c(0, 0, 0),
  n_images = length(labels),
  dimension_padding = c(1, 1),
  labels = c(0, 2),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, type, ndim, padding = header_padding)
  writeBin(
    as.integer(c(n_images, dimension_padding)),
    f,
    size = 4,
    endian = "little"
  )
  writeBin(as.integer(labels), f, size = 4, endian = "little")
  writeBin(as.integer(trailing), f, size = 4, endian = "little")
}

write_gz_norb_info <- function(
  path,
  type = "integer",
  ndim = 2,
  header_padding = c(0, 0, 0),
  n_images = 2,
  n_features = 4,
  dimension_padding = 1,
  values = seq_len(n_images * n_features),
  trailing = integer()
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, type, ndim, padding = header_padding)
  writeBin(
    as.integer(c(n_images, n_features, dimension_padding)),
    f,
    size = 4,
    endian = "little"
  )
  writeBin(as.integer(values), f, size = 4, endian = "little")
  writeBin(as.integer(trailing), f, size = 4, endian = "little")
}

test_that("MNIST parsers read small local gzip fixtures", {
  tmpdir <- tempdir()
  write_gz_mnist_images(file.path(tmpdir, "images.gz"))
  write_gz_mnist_labels(file.path(tmpdir, "labels.gz"))

  images <- snedata:::parse_image_file(
    "images.gz",
    base_url = file_base_url(tmpdir)
  )
  labels <- snedata:::parse_label_file(
    "labels.gz",
    base_url = file_base_url(tmpdir)
  )

  expect_equal(dim(images), c(2L, 4L))
  expect_equal(images[1, ], 1:4)
  expect_equal(images[2, ], 5:8)
  expect_equal(labels, c(0L, 5L, 9L))
})

test_that("MNIST parsers reject truncated IDX headers and payloads", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_mnist_header(file.path(tmpdir, "short-images.gz"), c(2051, 2, 2))
  write_gz_mnist_header(file.path(tmpdir, "short-labels.gz"), 2049)
  write_gz_mnist_images(
    file.path(tmpdir, "short-image-payload.gz"),
    pixels = 1:7
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "short-label-payload.gz"),
    n_labels = 3,
    labels = c(0, 5)
  )

  expect_error(
    snedata:::parse_image_file(
      "short-images.gz",
      base_url = file_base_url(tmpdir)
    ),
    "MNIST image asset 'short-images.gz'.*column count.*expected count 1 values \\(4 bytes\\); actual count 0"
  )
  expect_error(
    snedata:::parse_label_file(
      "short-labels.gz",
      base_url = file_base_url(tmpdir)
    ),
    "MNIST label asset 'short-labels.gz'.*label count.*expected count 1 values \\(4 bytes\\); actual count 0"
  )
  expect_error(
    snedata:::parse_image_file(
      "short-image-payload.gz",
      base_url = file_base_url(tmpdir)
    ),
    "image payload.*expected count 8 values \\(8 bytes\\); actual count 7.*n_images=2, n_rows=2, n_cols=2"
  )
  expect_error(
    snedata:::parse_label_file(
      "short-label-payload.gz",
      base_url = file_base_url(tmpdir)
    ),
    "label payload.*expected count 3 values \\(3 bytes\\); actual count 2.*n_labels=3"
  )
})

test_that("MNIST parsers reject trailing IDX data and invalid magic numbers", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_mnist_images(file.path(tmpdir, "trailing-images.gz"), trailing = 9)
  write_gz_mnist_labels(file.path(tmpdir, "trailing-labels.gz"), trailing = 9)
  write_gz_mnist_images(file.path(tmpdir, "bad-images.gz"), magic = 1)
  write_gz_mnist_labels(file.path(tmpdir, "bad-labels.gz"), magic = 1)

  expect_error(
    snedata:::parse_image_file(
      "trailing-images.gz",
      base_url = file_base_url(tmpdir)
    ),
    "trailing data.*expected count 0 values \\(0 bytes\\); actual count 1.*n_images=2, n_rows=2, n_cols=2"
  )
  expect_error(
    snedata:::parse_label_file(
      "trailing-labels.gz",
      base_url = file_base_url(tmpdir)
    ),
    "trailing data.*expected count 0 values \\(0 bytes\\); actual count 1.*n_labels=3"
  )
  expect_error(
    snedata:::parse_image_file(
      "bad-images.gz",
      base_url = file_base_url(tmpdir)
    ),
    "invalid magic number: expected 2051.*actual 1"
  )
  expect_error(
    snedata:::parse_label_file(
      "bad-labels.gz",
      base_url = file_base_url(tmpdir)
    ),
    "invalid magic number: expected 2049.*actual 1"
  )
})

test_that("MNIST parsers reject invalid and overflow-sized IDX dimensions", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_mnist_images(
    file.path(tmpdir, "zero-images.gz"),
    n_images = 0,
    pixels = integer()
  )
  write_gz_mnist_images(
    file.path(tmpdir, "negative-images.gz"),
    n_images = -1,
    pixels = integer()
  )
  write_gz_mnist_images(
    file.path(tmpdir, "overflow-images.gz"),
    n_images = .Machine$integer.max,
    pixels = integer()
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "zero-labels.gz"),
    n_labels = 0,
    labels = integer()
  )

  expect_error(
    snedata:::parse_image_file("zero-images.gz", file_base_url(tmpdir)),
    "invalid dimensions.*n_images=0, n_rows=2, n_cols=2"
  )
  expect_error(
    snedata:::parse_image_file("negative-images.gz", file_base_url(tmpdir)),
    "invalid dimensions.*n_images=-1, n_rows=2, n_cols=2"
  )
  expect_error(
    snedata:::parse_image_file("overflow-images.gz", file_base_url(tmpdir)),
    "impossible payload size.*n_images=2147483647, n_rows=2, n_cols=2"
  )
  expect_error(
    snedata:::parse_label_file("zero-labels.gz", file_base_url(tmpdir)),
    "invalid dimensions.*n_labels=0"
  )
})

test_that("MNIST reports image and label count mismatches from local fixtures", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_mnist_images(file.path(tmpdir, "images.gz"))
  write_gz_mnist_labels(file.path(tmpdir, "labels.gz"), labels = c(0, 5, 9))

  expect_error(
    snedata:::parse_files(
      "images.gz",
      "labels.gz",
      base_url = file_base_url(tmpdir)
    ),
    "image/label count mismatch.*image asset 'images.gz'.*expected count 2.*label asset 'labels.gz'.*actual count 3"
  )
})

test_that("MNIST downloader can return the original data frame or a canonical list", {
  tmpdir <- tempfile()
  dir.create(tmpdir)

  write_gz_mnist_images(
    file.path(tmpdir, "train-images-idx3-ubyte.gz"),
    pixels = 1:8
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "train-labels-idx1-ubyte.gz"),
    labels = c(1, 2)
  )
  write_gz_mnist_images(
    file.path(tmpdir, "t10k-images-idx3-ubyte.gz"),
    pixels = 9:16
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "t10k-labels-idx1-ubyte.gz"),
    labels = c(3, 4)
  )

  with_mocked_bindings(
    validate_mnist_dataset = function(...) invisible(NULL),
    {
      df <- download_mnist(base_url = file_base_url(tmpdir))
      mat <- download_mnist(base_url = file_base_url(tmpdir), as = "list")
    },
    .package = "snedata"
  )

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c(paste0("px", 1:4), "Label"))
  expect_equal(dim(df), c(4L, 5L))
  expect_named(mat, c("data", "meta", "image_dim", "channel_order", "source"))
  expect_equal(dim(mat$data), c(4L, 4L))
  expect_equal(unname(mat$data[1, ]), 1:4)
  expect_equal(unname(mat$data[4, ]), 13:16)
  expect_equal(as.character(mat$meta$label), as.character(1:4))
  expect_equal(levels(mat$meta$label), as.character(1:4))
  expect_equal(
    as.character(mat$meta$split),
    c("training", "training", "testing", "testing")
  )
})

test_that("MNIST dataset validation enforces named split contracts", {
  make_split <- function(split, labels = c(0L, 9L)) {
    snedata:::format_image_label_result(
      structure(
        matrix(1:8, nrow = 2),
        image_dim = c(height = 2L, width = 2L)
      ),
      labels,
      split = split,
      source = list(dataset = "Fixture-MNIST", url = "local")
    )
  }
  train <- make_split("training")
  test <- make_split("testing")
  expected_counts <- c(training = 2L, testing = 2L)
  expected_image_dim <- c(height = 2L, width = 2L)

  expect_silent(snedata:::validate_mnist_dataset(
    train,
    test,
    dataset = "Fixture-MNIST",
    expected_counts = expected_counts,
    expected_image_dim = expected_image_dim
  ))

  bad_dimensions <- train
  bad_dimensions$image_dim <- c(height = 1L, width = 4L)
  expect_error(
    snedata:::validate_mnist_dataset(
      bad_dimensions,
      test,
      dataset = "Fixture-MNIST",
      expected_counts = expected_counts,
      expected_image_dim = expected_image_dim
    ),
    "Fixture-MNIST training field 'image_dim'.*allowed height=2, width=2; observed height=1, width=4"
  )

  expect_error(
    snedata:::validate_mnist_dataset(
      train,
      test,
      dataset = "Fixture-MNIST",
      expected_counts = c(training = 3L, testing = 2L),
      expected_image_dim = expected_image_dim
    ),
    "Fixture-MNIST training field 'row_count'.*allowed 3; observed 2"
  )

  bad_labels <- make_split("training", labels = c(0L, 12L))
  expect_error(
    snedata:::validate_mnist_dataset(
      bad_labels,
      test,
      dataset = "Fixture-MNIST",
      expected_counts = expected_counts,
      expected_image_dim = expected_image_dim
    ),
    "Fixture-MNIST training field 'label'.*allowed 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; observed 12"
  )
})

test_that("MNIST-family public downloaders request canonical specifications", {
  observed_counts <- list()
  parse_fixture <- function(..., split, dataset) {
    snedata:::new_image_result(
      matrix(0L, nrow = 1L),
      meta = data.frame(
        id = 1L,
        split = factor(split, levels = c("training", "testing")),
        label = factor("0")
      ),
      image_dim = c(height = 1L, width = 1L),
      channel_order = "gray",
      source = list(dataset = dataset, url = "local")
    )
  }

  with_mocked_bindings(
    parse_files = parse_fixture,
    validate_mnist_dataset = function(
      train,
      test,
      dataset,
      expected_counts,
      ...
    ) {
      observed_counts[[dataset]] <<- expected_counts
      invisible(NULL)
    },
    {
      download_mnist(as = "list")
      download_fashion_mnist(as = "list")
      download_kuzushiji_mnist(as = "list")
      download_qmnist(as = "list")
    },
    .package = "snedata"
  )

  expect_equal(
    names(observed_counts),
    c("MNIST", "Fashion-MNIST", "Kuzushiji-MNIST", "QMNIST")
  )
  expect_equal(
    observed_counts[c("MNIST", "Fashion-MNIST", "Kuzushiji-MNIST")],
    stats::setNames(
      rep(list(c(training = 60000L, testing = 10000L)), 3L),
      c("MNIST", "Fashion-MNIST", "Kuzushiji-MNIST")
    )
  )
  expect_equal(
    observed_counts$QMNIST,
    c(training = 60000L, testing = 60000L)
  )
})

test_that("Fashion-MNIST descriptions are mapped by digit label value", {
  tmpdir <- tempfile()
  dir.create(tmpdir)

  write_gz_mnist_images(
    file.path(tmpdir, "train-images-idx3-ubyte.gz"),
    pixels = 1:8
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "train-labels-idx1-ubyte.gz"),
    labels = c(0, 9)
  )
  write_gz_mnist_images(
    file.path(tmpdir, "t10k-images-idx3-ubyte.gz"),
    pixels = 9:16
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "t10k-labels-idx1-ubyte.gz"),
    labels = c(1, 8)
  )

  with_mocked_bindings(
    validate_mnist_dataset = function(...) invisible(NULL),
    fashion <- download_fashion_mnist(base_url = file_base_url(tmpdir)),
    .package = "snedata"
  )

  expect_equal(names(fashion), c(paste0("px", 1:4), "Label", "Description"))
  expect_equal(as.character(fashion$Label), c("0", "9", "1", "8"))
  expect_equal(
    as.character(fashion$Description),
    c("T-shirt/top", "Ankle boot", "Trouser", "Bag")
  )
})

test_that("QMNIST extended label parser preserves named metadata", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_qmnist_labels(file.path(tmpdir, "labels.gz"))

  labels <- snedata:::parse_extended_label_file(
    "labels.gz",
    base_url = file_base_url(tmpdir)
  )
  metadata <- attr(labels, "qmnist_metadata")

  expect_equal(as.integer(labels), c(7L, 4L))
  expect_equal(
    colnames(metadata),
    c(
      "character_class",
      "nist_hsf_series",
      "nist_writer_id",
      "digit_index",
      "nist_class_code",
      "nist_global_digit_index",
      "duplicate",
      "unused"
    )
  )
  expect_equal(metadata[, "nist_writer_id"], c(610L, 2100L))
  expect_equal(metadata[, "nist_global_digit_index"], c(1001L, 1002L))
})

test_that("QMNIST uses the archived repository's main branch", {
  expect_identical(
    snedata:::qmnist_url,
    "https://github.com/facebookresearch/qmnist/raw/refs/heads/main/"
  )
})

test_that("QMNIST extended label parser validates the exact IDX2 payload", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_mnist_header(file.path(tmpdir, "short-header.gz"), c(3074, 2))
  write_gz_qmnist_labels(file.path(tmpdir, "bad-magic.gz"), magic = 2049)
  write_gz_qmnist_labels(
    file.path(tmpdir, "wrong-columns.gz"),
    metadata = matrix(1:6, nrow = 2, byrow = TRUE)
  )
  write_gz_qmnist_labels(
    file.path(tmpdir, "short-row.gz"),
    values = seq_len(7)
  )
  write_gz_qmnist_labels(
    file.path(tmpdir, "short-payload.gz"),
    values = seq_len(15)
  )
  write_gz_qmnist_labels(file.path(tmpdir, "trailing.gz"), trailing = 99)

  expect_error(
    snedata:::parse_extended_label_file(
      "short-header.gz",
      base_url = file_base_url(tmpdir)
    ),
    "QMNIST extended label asset 'short-header.gz'.*column count.*expected count 1 values \\(4 bytes\\); actual count 0"
  )
  expect_error(
    snedata:::parse_extended_label_file(
      "bad-magic.gz",
      base_url = file_base_url(tmpdir)
    ),
    "invalid magic number: expected 3074.*actual 2049"
  )
  expect_error(
    snedata:::parse_extended_label_file(
      "wrong-columns.gz",
      base_url = file_base_url(tmpdir)
    ),
    "invalid column count.*expected count 8 columns \\(32 bytes per row\\); actual count 3.*n_rows=2, n_cols=3"
  )
  expect_error(
    snedata:::parse_extended_label_file(
      "short-row.gz",
      base_url = file_base_url(tmpdir)
    ),
    "extended label payload.*expected count 16 values \\(64 bytes\\); actual count 7.*n_rows=2, n_cols=8"
  )
  expect_error(
    snedata:::parse_extended_label_file(
      "short-payload.gz",
      base_url = file_base_url(tmpdir)
    ),
    "extended label payload.*expected count 16 values \\(64 bytes\\); actual count 15.*n_rows=2, n_cols=8"
  )
  expect_error(
    snedata:::parse_extended_label_file(
      "trailing.gz",
      base_url = file_base_url(tmpdir)
    ),
    "trailing data.*expected count 0 values \\(0 bytes\\); actual count 1.*n_rows=2, n_cols=8"
  )
})

test_that("QMNIST downloader can return a canonical list", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  train_metadata <- rbind(
    c(7, 0, 610, 12, 37, 1001, 0, 0),
    c(4, 4, 2100, 13, 38, 1002, 0, 0)
  )
  test_metadata <- rbind(
    c(1, 1, 711, 14, 39, 1003, 1, 0),
    c(0, 2, 812, 15, 40, 1004, 0, 0)
  )

  write_gz_mnist_images(
    file.path(tmpdir, "qmnist-train-images-idx3-ubyte.gz"),
    pixels = 1:8
  )
  write_gz_qmnist_labels(
    file.path(tmpdir, "qmnist-train-labels-idx2-int.gz"),
    metadata = train_metadata
  )
  write_gz_mnist_images(
    file.path(tmpdir, "qmnist-test-images-idx3-ubyte.gz"),
    pixels = 9:16
  )
  write_gz_qmnist_labels(
    file.path(tmpdir, "qmnist-test-labels-idx2-int.gz"),
    metadata = test_metadata
  )

  with_mocked_bindings(
    validate_mnist_dataset = function(...) invisible(NULL),
    mat <- download_qmnist(base_url = file_base_url(tmpdir), as = "list"),
    .package = "snedata"
  )

  expect_named(mat, c("data", "meta", "image_dim", "channel_order", "source"))
  expect_equal(dim(mat$data), c(4L, 4L))
  expect_equal(unname(mat$data[3, ]), 9:12)
  expect_equal(as.character(mat$meta$label), c("7", "4", "1", "0"))
  expect_equal(names(mat$meta)[4:11], snedata:::qmnist_extended_label_columns)
  expect_false(anyNA(mat$meta[snedata:::qmnist_extended_label_columns]))
  expect_equal(
    unname(as.matrix(mat$meta[snedata:::qmnist_extended_label_columns])),
    rbind(train_metadata, test_metadata)
  )
})

test_that("downloaders expose a configurable timeout", {
  expect_identical(
    formals(download_cifar10)$url,
    "https://cave.cs.toronto.edu/kriz/cifar-10-binary.tar.gz"
  )
  downloaders <- list(
    cifar10 = download_cifar10,
    coil20 = download_coil20,
    coil100 = download_coil100,
    fashion_mnist = download_fashion_mnist,
    isomap_faces = download_isomap_faces,
    isomap_swiss_roll = download_isomap_swiss_roll,
    kuzushiji_mnist = download_kuzushiji_mnist,
    mammoth10k = download_mammoth10k,
    mammoth50k = download_mammoth50k,
    mnist = download_mnist,
    norb_small = download_norb_small,
    qmnist = download_qmnist,
    twenty_newsgroups = download_twenty_newsgroups
  )
  defaults <- vapply(
    downloaders,
    function(downloader) formals(downloader)$timeout,
    numeric(1)
  )

  expect_identical(unname(defaults), rep(1800, length(downloaders)))
})

test_that("download timeout is raised and restored", {
  original_timeout <- getOption("timeout")
  on.exit(options(timeout = original_timeout), add = TRUE)

  expect_identical(
    snedata:::with_download_timeout(getOption("timeout"), timeout = 123),
    123
  )
  expect_identical(getOption("timeout"), original_timeout)

  options(timeout = 2400)
  expect_identical(
    snedata:::with_download_timeout(getOption("timeout"), timeout = 1800),
    2400
  )
  expect_identical(getOption("timeout"), 2400)
  expect_error(
    snedata:::with_download_timeout(NULL, timeout = 0),
    "positive finite number of seconds"
  )
})

test_that("MNIST keeps its timeout while streaming files", {
  observed <- numeric()

  with_mocked_bindings(
    parse_files = function(...) {
      observed <<- c(observed, getOption("timeout"))
      snedata:::new_image_result(
        matrix(0L, nrow = 1L),
        meta = data.frame(
          id = 1L,
          split = factor("training", levels = c("training", "testing")),
          label = factor("0")
        ),
        image_dim = c(height = 1L, width = 1L),
        channel_order = "gray",
        source = list(dataset = "MNIST", url = "unused")
      )
    },
    validate_mnist_dataset = function(...) invisible(NULL),
    download_mnist(as = "list", timeout = 123),
    .package = "snedata"
  )

  expect_identical(observed, c(123, 123))
})

test_that("CIFAR forwards its configurable download timeout", {
  observed <- new.env(parent = emptyenv())

  with_mocked_bindings(
    download_asset = function(url, destfile, verbose, timeout) {
      observed$timeout <- timeout
      stop("stop before download")
    },
    expect_error(
      download_cifar10(as = "list", timeout = 123),
      "stop before download"
    ),
    .package = "snedata"
  )

  expect_identical(observed$timeout, 123)
  expect_error(
    snedata:::download_asset("unused", tempfile(), timeout = 0),
    "positive finite number of seconds"
  )
})

test_that("CIFAR parser rejects incomplete fixed-size batches", {
  path <- tempfile()
  f <- file(path, "wb")
  writeBin(as.integer(1:10), f, size = 1)
  close(f)

  expect_error(
    snedata:::read_cifar_bin(path),
    "Expected 30730000 bytes"
  )
})

test_that("CIFAR parser returns rows in file record order", {
  path <- tempfile()
  f <- file(path, "wb")
  writeBin(as.integer(c(7, 1, 2, 3, 8, 4, 5, 6)), f, size = 1)
  close(f)

  batch <- snedata:::read_cifar_bin(path, n_images = 2, n_pixels = 3)

  expect_equal(batch$labels, c(7L, 8L))
  expect_equal(batch$images, matrix(1:6, nrow = 2, byrow = TRUE))
})

test_that("CIFAR formatter can return a data frame or a canonical list", {
  images <- matrix(rep(1:255, length.out = 2L * 3072L), nrow = 2)
  labels <- c(0, 9)

  df <- snedata:::format_cifar_result(images, labels)
  mat <- snedata:::format_cifar_result(images, labels, as = "list")

  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df)[c(1, 1024, 1025, 2048, 2049, 3072, 3073, 3074)],
    c("r1", "r1024", "g1", "g1024", "b1", "b1024", "Label", "Description")
  )
  expect_equal(as.character(df$Description), c("airplane", "truck"))
  expect_named(mat, c("data", "meta", "image_dim", "channel_order", "source"))
  expect_equal(dim(mat$data), c(2L, 3072L))
  expect_equal(as.character(mat$meta$label), c("0", "9"))
  expect_equal(as.character(mat$meta$description), c("airplane", "truck"))
})

test_that("CIFAR validation rejects wrong pixel counts and label values", {
  expect_error(
    snedata:::format_cifar_result(matrix(1:12, nrow = 2), c(0L, 9L)),
    "CIFAR-10 all field 'pixel_count'.*allowed 3072; observed 6"
  )

  images <- matrix(0L, nrow = 2L, ncol = 3072L)
  expect_error(
    snedata:::format_cifar_result(
      images,
      c(0L, 10L),
      split = c("training", "testing")
    ),
    "CIFAR-10 testing field 'label'.*allowed 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; observed 10"
  )
  expect_error(
    snedata:::format_cifar_result(images, c(0L, 9L), split = "training"),
    "CIFAR-10 all field 'split_count'.*allowed 2; observed 1"
  )
  expect_error(
    snedata:::format_cifar_result(
      images,
      c(0L, 9L),
      split = c("training", "validation")
    ),
    "CIFAR-10 all field 'split'.*allowed training, testing; observed validation"
  )
})

test_that("tar entry validation rejects unsafe and duplicate paths", {
  expect_silent(snedata:::validate_tar_entries("root/file", "fixture"))
  expect_error(
    snedata:::validate_tar_entries("../file", "fixture"),
    "unsafe archive paths"
  )
  expect_error(
    snedata:::validate_tar_entries("/tmp/file", "fixture"),
    "unsafe archive paths"
  )
  expect_error(
    snedata:::validate_tar_entries("C:/tmp/file", "fixture"),
    "unsafe archive paths"
  )
  expect_error(
    snedata:::validate_tar_entries(c("root/file", "root\\\\file"), "fixture"),
    "duplicate archive paths"
  )
})

test_that("tar extraction rejects symbolic links before extraction", {
  tarball <- tempfile(fileext = ".tar.gz")
  write_gz_tar_link_fixture(tarball)

  expect_error(
    snedata:::extract_tar_safely(
      tarball,
      tempfile(),
      asset = "fixture",
      validate_layout = function(entries, asset) NULL
    ),
    "unsafe tar link entry"
  )
})

test_that("CIFAR archive layout requires the complete expected file set", {
  entries <- c(
    paste0("cifar-10-batches-bin/data_batch_", 1:5, ".bin"),
    "cifar-10-batches-bin/test_batch.bin",
    "cifar-10-batches-bin/batches.meta.txt",
    "cifar-10-batches-bin/readme.html"
  )

  expect_silent(snedata:::validate_cifar_archive_layout(entries, "fixture"))
  expect_error(
    snedata:::validate_cifar_archive_layout(entries[-1], "fixture"),
    "missing 1"
  )
  expect_error(
    snedata:::validate_cifar_archive_layout(
      c(entries, "cifar-10-batches-bin/extra.bin"),
      "fixture"
    ),
    "unexpected 1"
  )
})

test_that("CIFAR downloader cleans only its owned paths after archive errors", {
  source_root <- tempfile()
  dir.create(file.path(source_root, "wrong-root"), recursive = TRUE)
  writeLines("fixture", file.path(source_root, "wrong-root", "file"))
  tarball <- tempfile(fileext = ".tar.gz")
  oldwd <- setwd(source_root)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(tarball, "wrong-root", compression = "gzip", tar = "internal")

  parent <- tempfile()
  dir.create(parent)
  sentinel <- file.path(parent, "caller-owned.txt")
  writeLines("keep", sentinel)
  destfile <- file.path(parent, "cifar")

  expect_error(
    download_cifar10(
      url = paste0("file://", normalizePath(tarball, winslash = "/")),
      destfile = destfile,
      cleanup = TRUE,
      as = "list"
    ),
    "CIFAR-10 archive has an invalid layout"
  )
  expect_true(file.exists(sentinel))
  expect_false(file.exists(paste0(destfile, ".tar.gz")))
  expect_false(any(grepl("^cifar10-", list.files(parent))))
})

test_that("CIFAR downloader retains owned paths when cleanup is disabled", {
  source_root <- tempfile()
  dir.create(file.path(source_root, "wrong-root"), recursive = TRUE)
  writeLines("fixture", file.path(source_root, "wrong-root", "file"))
  tarball <- tempfile(fileext = ".tar.gz")
  oldwd <- setwd(source_root)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(tarball, "wrong-root", compression = "gzip", tar = "internal")

  parent <- tempfile()
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  destfile <- file.path(parent, "cifar")

  expect_error(
    download_cifar10(
      url = paste0("file://", normalizePath(tarball, winslash = "/")),
      destfile = destfile,
      cleanup = FALSE,
      as = "list"
    ),
    "CIFAR-10 archive has an invalid layout"
  )
  expect_true(file.exists(paste0(destfile, ".tar.gz")))
  expect_true(any(grepl("^cifar10-", list.files(parent))))
})

test_that("NORB parsers read small local gzip fixtures through base_url", {
  tmpdir <- tempdir()
  image_values <- rep(1:255, length.out = 2 * 2 * 96 * 96)
  write_gz_norb_images(
    file.path(tmpdir, "images.mat.gz"),
    values = image_values
  )
  write_gz_norb_categories(
    file.path(tmpdir, "categories.mat.gz"),
    labels = c(0, 2)
  )
  write_gz_norb_info(file.path(tmpdir, "info.mat.gz"))

  images <- snedata:::read_norb_images(
    file = "images.mat.gz",
    base_url = file_base_url(tmpdir),
    verbose = FALSE
  )
  categories <- snedata:::read_norb_categories(
    file = "categories.mat.gz",
    base_url = file_base_url(tmpdir),
    verbose = FALSE
  )
  info <- snedata:::read_norb_info(
    file = "info.mat.gz",
    base_url = file_base_url(tmpdir),
    verbose = FALSE
  )

  expect_equal(dim(images), c(2L, 18432L))
  expect_equal(images[1, 1:4], 1:4)
  expect_equal(images[2, 1:4], 73:76)
  expect_equal(images, t(matrix(image_values, nrow = 18432L, ncol = 2L)))
  expect_equal(categories, c(0L, 2L))
  expect_equal(dim(info), c(4L, 2L))
  expect_equal(info[, 1], 1:4)
  expect_equal(info[, 2], 5:8)
})

test_that("NORB parsers reject malformed headers, dimensions, and payloads", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gz_norb_images(
    file.path(tmpdir, "short-dimensions.mat.gz"),
    dimensions = integer(),
    values = integer()
  )
  write_gz_norb_images(
    file.path(tmpdir, "bad-magic.mat.gz"),
    type = "integer"
  )
  write_gz_norb_images(
    file.path(tmpdir, "bad-header-padding.mat.gz"),
    header_padding = c(1, 0, 0)
  )
  write_gz_norb_images(
    file.path(tmpdir, "bad-shape.mat.gz"),
    dimensions = c(2, 1, 96, 96),
    values = integer()
  )
  write_gz_norb_images(
    file.path(tmpdir, "same-product-bad-shape.mat.gz"),
    dimensions = c(2, 1, 96, 192),
    values = integer()
  )
  write_gz_norb_images(
    file.path(tmpdir, "overflow.mat.gz"),
    dimensions = c(.Machine$integer.max, 2, 96, 96),
    values = integer()
  )
  write_gz_norb_images(
    file.path(tmpdir, "short-image-payload.mat.gz"),
    values = seq_len(10)
  )
  write_gz_norb_images(
    file.path(tmpdir, "trailing-images.mat.gz"),
    trailing = 1
  )
  write_gz_norb_categories(
    file.path(tmpdir, "bad-category-padding.mat.gz"),
    dimension_padding = c(2, 1)
  )
  write_gz_norb_categories(
    file.path(tmpdir, "short-categories.mat.gz"),
    n_images = 2,
    labels = 0
  )
  write_gz_norb_categories(
    file.path(tmpdir, "trailing-categories.mat.gz"),
    trailing = 1
  )
  write_gz_norb_info(file.path(tmpdir, "bad-info-ndim.mat.gz"), ndim = 1)
  write_gz_norb_info(
    file.path(tmpdir, "bad-info-features.mat.gz"),
    n_features = 3,
    values = 1:6
  )
  write_gz_norb_info(
    file.path(tmpdir, "bad-info-padding.mat.gz"),
    dimension_padding = 2
  )
  write_gz_norb_info(
    file.path(tmpdir, "short-info-payload.mat.gz"),
    values = 1:7
  )
  write_gz_norb_info(file.path(tmpdir, "trailing-info.mat.gz"), trailing = 1)

  expect_error(
    snedata:::read_norb_images(
      file = "short-dimensions.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "dimension values.*expected count 4 values \\(16 bytes\\); actual count 0"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "bad-magic.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid matrix magic: expected byte matrix"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "bad-header-padding.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid padding.*actual count 1 nonzero values"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "bad-shape.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid image dimensions.*expected n_cameras=2, n_rows=96, n_cols=96 \\(18432 pixels per image\\); actual n_cameras=1, n_rows=96, n_cols=96 \\(9216 pixels per image\\)"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "same-product-bad-shape.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid image dimensions.*expected n_cameras=2, n_rows=96, n_cols=96 \\(18432 pixels per image\\); actual n_cameras=1, n_rows=96, n_cols=192 \\(18432 pixels per image\\)"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "overflow.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "impossible payload size.*n_images=2147483647"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "short-image-payload.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "image payload.*expected count 36864 values \\(36864 bytes\\); actual count 10"
  )
  expect_error(
    snedata:::read_norb_images(
      file = "trailing-images.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "trailing data.*n_images=2, n_cameras=2, n_rows=96, n_cols=96"
  )
  expect_error(
    snedata:::read_norb_categories(
      file = "bad-category-padding.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid category dimensions.*expected n_category_columns=1, n_category_planes=1; actual n_category_columns=2, n_category_planes=1"
  )
  expect_error(
    snedata:::read_norb_categories(
      file = "short-categories.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "category payload.*expected count 2 values \\(8 bytes\\); actual count 1"
  )
  expect_error(
    snedata:::read_norb_categories(
      file = "trailing-categories.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "trailing data.*n_images=2, n_category_columns=1, n_category_planes=1"
  )
  expect_error(
    snedata:::read_norb_info(
      file = "bad-info-ndim.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid dimension count: expected count 2 dimensions.*actual count 1"
  )
  expect_error(
    snedata:::read_norb_info(
      file = "bad-info-features.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid metadata feature count.*expected count 4 features.*actual count 3"
  )
  expect_error(
    snedata:::read_norb_info(
      file = "bad-info-padding.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "invalid metadata dimensions.*expected n_metadata_planes=1; actual n_metadata_planes=2"
  )
  expect_error(
    snedata:::read_norb_info(
      file = "short-info-payload.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "metadata payload.*expected count 8 values \\(32 bytes\\); actual count 7"
  )
  expect_error(
    snedata:::read_norb_info(
      file = "trailing-info.mat.gz",
      base_url = file_base_url(tmpdir),
      verbose = FALSE
    ),
    "trailing data.*n_images=2, n_features=4, n_metadata_planes=1"
  )
})

test_that("NORB validates image, category, and metadata counts before formatting", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  base <- "smallnorb-5x46789x9x18x6x2x96x96-training"
  write_gz_norb_images(file.path(tmpdir, paste0(base, "-dat.mat.gz")))
  write_gz_norb_categories(
    file.path(tmpdir, paste0(base, "-cat.mat.gz")),
    labels = c(0, 2, 4)
  )
  write_gz_norb_info(file.path(tmpdir, paste0(base, "-info.mat.gz")))

  expect_error(
    download_norb_small(
      base_url = file_base_url(tmpdir),
      split = "training",
      as = "list"
    ),
    "NORB training asset count mismatch: image count 2, info count 2, category count 3"
  )
})

test_that("NORB formatter can return a data frame or a canonical list", {
  images <- matrix(
    rep(1:255, length.out = 2L * 96L * 96L * 2L),
    nrow = 2,
    byrow = TRUE
  )
  info <- matrix(
    c(
      1,
      2,
      4,
      5,
      5,
      6,
      8,
      0
    ),
    nrow = 4
  )
  cats <- c(0, 4)

  df <- snedata:::format_norb_result(
    images,
    info,
    cats,
    split = "training"
  )
  mat <- snedata:::format_norb_result(
    images,
    info,
    cats,
    split = "training",
    as = "list"
  )

  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df)[c(1, 2, 9216, 9217, 18432)],
    c("c0px1", "c0px2", "c0px9216", "c1px1", "c1px9216")
  )
  expect_equal(
    tail(names(df), 7),
    c(
      "Instance",
      "Elevation",
      "Azimuth",
      "Lighting",
      "Split",
      "Label",
      "Description"
    )
  )
  expect_equal(dim(mat$data), c(2L, 18432L))
  expect_named(mat, c("data", "meta", "image_dim", "channel_order", "source"))
  expect_equal(
    names(mat$meta),
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
  expect_equal(as.character(mat$meta$description), c("Animal", "Car"))
})

test_that("NORB validation rejects wrong counts and out-of-range metadata", {
  images <- matrix(0L, nrow = 1L, ncol = 96L * 96L * 2L)
  info <- matrix(c(0L, 0L, 0L, 0L), nrow = 4L)

  expect_error(
    snedata:::format_norb_result(
      images,
      info,
      0L,
      split = "training",
      expected_count = 2L
    ),
    "Small NORB training field 'row_count'.*allowed 2; observed 1"
  )

  invalid_metadata <- list(
    list(row = 1L, field = "instance", value = 10L),
    list(row = 2L, field = "elevation", value = 9L),
    list(row = 3L, field = "azimuth", value = 3L),
    list(row = 4L, field = "lighting", value = 6L)
  )
  for (case in invalid_metadata) {
    bad_info <- info
    bad_info[case$row, 1] <- case$value
    expect_error(
      snedata:::format_norb_result(images, bad_info, 0L, split = "training"),
      paste0(
        "Small NORB training field '",
        case$field,
        "'.*observed ",
        case$value
      )
    )
  }
  expect_error(
    snedata:::format_norb_result(images, info, 5L, split = "testing"),
    "Small NORB testing field 'category'.*allowed 0, 1, 2, 3, 4; observed 5"
  )
  expect_error(
    snedata:::format_norb_result(images, info, 0L, split = "validation"),
    "Small NORB all field 'split'.*allowed training, testing; observed validation"
  )
})

test_that("NORB downloader assembles requested splits from local fixtures", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  base_url <- file_base_url(tmpdir)

  training_base <- "smallnorb-5x46789x9x18x6x2x96x96-training"
  testing_base <- "smallnorb-5x01235x9x18x6x2x96x96-testing"

  write_gz_norb_images(file.path(tmpdir, paste0(training_base, "-dat.mat.gz")))
  write_gz_norb_categories(
    file.path(tmpdir, paste0(training_base, "-cat.mat.gz")),
    labels = c(0, 4)
  )
  write_gz_norb_info(
    file.path(tmpdir, paste0(training_base, "-info.mat.gz")),
    values = c(0, 0, 0, 0, 1, 1, 2, 1)
  )

  write_gz_norb_images(file.path(tmpdir, paste0(testing_base, "-dat.mat.gz")))
  write_gz_norb_categories(
    file.path(tmpdir, paste0(testing_base, "-cat.mat.gz")),
    labels = c(2, 3)
  )
  write_gz_norb_info(
    file.path(tmpdir, paste0(testing_base, "-info.mat.gz")),
    values = c(4, 2, 4, 2, 6, 3, 6, 3)
  )

  expect_error(
    download_norb_small(
      base_url = base_url,
      split = "training",
      as = "list"
    ),
    "Small NORB training field 'row_count'.*allowed 24300; observed 2"
  )

  training <- snedata:::read_norb_data(
    base_url = base_url,
    split = "training",
    as = "list",
    verbose = FALSE,
    expected_count = 2L
  )
  all <- snedata:::read_norb_all_data(
    base_url = base_url,
    verbose = FALSE,
    as = "list",
    expected_count = 2L
  )
  all_df <- NULL
  expect_no_warning(
    all_df <- snedata:::read_norb_all_data(
      base_url = base_url,
      verbose = FALSE,
      as = "data.frame",
      expected_count = 2L
    )
  )

  expect_equal(dim(training$data), c(2L, 18432L))
  expect_identical(training$source$url, base_url)
  expect_equal(as.character(training$meta$split), c("training", "training"))
  expect_equal(as.character(training$meta$description), c("Animal", "Car"))

  expect_equal(dim(all$data), c(4L, 18432L))
  expect_identical(all$source$url, base_url)
  expect_equal(
    as.character(all$meta$split),
    c("training", "training", "testing", "testing")
  )
  expect_equal(
    as.character(all$meta$description),
    c("Animal", "Car", "Airplane", "Truck")
  )
  expect_equal(dim(all_df), c(4L, 18439L))
  expect_equal(as.matrix(all_df[, 1:18432]), all$data)
})

test_that("20 Newsgroups downloader extracts a local tarball and removes it", {
  source_root <- tempfile()
  train_dir <- file.path(source_root, "20news-bydate-train", "alt.atheism")
  test_dir <- file.path(source_root, "20news-bydate-test", "alt.atheism")
  dir.create(train_dir, recursive = TRUE)
  dir.create(test_dir, recursive = TRUE)
  writeLines(c("Header: value", "", "Body"), file.path(train_dir, "1"))
  writeLines(c("Header: value", "", "Body"), file.path(test_dir, "2"))

  tarball <- tempfile(fileext = ".tar.gz")
  oldwd <- setwd(source_root)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(
    tarball,
    files = c("20news-bydate-train", "20news-bydate-test"),
    compression = "gzip",
    tar = "internal"
  )

  outdir <- tempfile()
  dir.create(outdir)
  snedata:::download_twenty_newsgroups_data(
    outdir,
    url = paste0("file://", normalizePath(tarball, winslash = "/"))
  )

  expect_true(file.exists(file.path(
    outdir,
    "20news-bydate-train",
    "alt.atheism",
    "1"
  )))
  expect_true(file.exists(file.path(
    outdir,
    "20news-bydate-test",
    "alt.atheism",
    "2"
  )))
  expect_length(list.files(outdir, pattern = "\\.tar\\.gz$"), 0)
})
