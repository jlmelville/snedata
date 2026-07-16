file_base_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE), "/")
}

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

write_norb_matrix_header <- function(f, type, ndim) {
  magic <- switch(
    type,
    byte = c(85L, 76L, 61L, 30L),
    integer = c(84L, 76L, 61L, 30L)
  )
  writeBin(as.integer(magic), f, size = 1)
  writeBin(as.integer(c(ndim, 0, 0, 0)), f, size = 1)
}

write_gz_norb_images <- function(path) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, "byte", ndim = 4)
  writeBin(as.integer(c(2, 1, 2, 2)), f, size = 4, endian = "little")
  writeBin(as.integer(1:8), f, size = 1)
}

write_gz_norb_categories <- function(path, labels = c(0, 2, 4)) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, "integer", ndim = 1)
  writeBin(as.integer(c(length(labels), 0, 0)), f, size = 4, endian = "little")
  writeBin(as.integer(labels), f, size = 4, endian = "little")
}

write_gz_norb_info <- function(path) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, "integer", ndim = 2)
  writeBin(as.integer(c(2, 4, 0)), f, size = 4, endian = "little")
  writeBin(as.integer(1:8), f, size = 4, endian = "little")
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

test_that("MNIST downloader can return the original data frame or a matrix list", {
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

  df <- download_mnist(base_url = file_base_url(tmpdir))
  mat <- download_mnist(base_url = file_base_url(tmpdir), as = "matrix")

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c(paste0("px", 1:4), "Label"))
  expect_equal(dim(df), c(4L, 5L))
  expect_named(mat, c("data", "labels"))
  expect_equal(dim(mat$data), c(4L, 4L))
  expect_equal(unname(mat$data[1, ]), 1:4)
  expect_equal(unname(mat$data[4, ]), 13:16)
  expect_equal(as.character(mat$labels), as.character(1:4))
  expect_equal(levels(mat$labels), as.character(1:4))
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

  fashion <- download_fashion_mnist(base_url = file_base_url(tmpdir))

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

test_that("QMNIST downloader can return a matrix list", {
  tmpdir <- tempfile()
  dir.create(tmpdir)

  write_gz_mnist_images(
    file.path(tmpdir, "qmnist-train-images-idx3-ubyte.gz"),
    pixels = 1:8
  )
  write_gz_qmnist_labels(
    file.path(tmpdir, "qmnist-train-labels-idx2-int.gz")
  )
  write_gz_mnist_images(
    file.path(tmpdir, "qmnist-test-images-idx3-ubyte.gz"),
    pixels = 9:16
  )
  write_gz_mnist_labels(
    file.path(tmpdir, "qmnist-test-labels-idx1-ubyte.gz"),
    labels = c(1, 0)
  )

  mat <- download_qmnist(base_url = file_base_url(tmpdir), as = "matrix")

  expect_named(mat, c("data", "labels"))
  expect_equal(dim(mat$data), c(4L, 4L))
  expect_equal(unname(mat$data[3, ]), 9:12)
  expect_equal(as.character(mat$labels), c("7", "4", "1", "0"))
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

test_that("CIFAR formatter can return a data frame or a matrix list", {
  images <- matrix(1:12, nrow = 2)
  labels <- c(0, 9)

  df <- snedata:::format_cifar_result(images, labels)
  mat <- snedata:::format_cifar_result(images, labels, as = "matrix")

  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df),
    c("r1", "r2", "g1", "g2", "b1", "b2", "Label", "Description")
  )
  expect_equal(as.character(df$Description), c("airplane", "truck"))
  expect_named(mat, c("data", "labels", "descriptions"))
  expect_equal(dim(mat$data), c(2L, 6L))
  expect_equal(as.character(mat$labels), c("0", "9"))
  expect_equal(as.character(mat$descriptions), c("airplane", "truck"))
})

test_that("NORB parsers read small local gzip fixtures through base_url", {
  tmpdir <- tempdir()
  write_gz_norb_images(file.path(tmpdir, "images.mat.gz"))
  write_gz_norb_categories(file.path(tmpdir, "categories.mat.gz"))
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

  expect_equal(dim(images), c(4L, 2L))
  expect_equal(images[, 1], 1:4)
  expect_equal(images[, 2], 5:8)
  expect_equal(categories, c(0L, 2L, 4L))
  expect_equal(dim(info), c(4L, 2L))
  expect_equal(info[, 1], 1:4)
  expect_equal(info[, 2], 5:8)
})

test_that("NORB formatter can return a data frame or a matrix list", {
  images <- matrix(1:8, nrow = 4)
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
    as = "matrix"
  )

  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df),
    c(
      "c0px1",
      "c0px2",
      "c1px1",
      "c1px2",
      "Instance",
      "Elevation",
      "Azimuth",
      "Lighting",
      "Split",
      "Label",
      "Description"
    )
  )
  expect_equal(dim(mat$data), c(2L, 4L))
  expect_named(mat, c("data", "meta"))
  expect_equal(names(mat$meta), names(df)[5:11])
  expect_equal(as.character(mat$meta$Description), c("Animal", "Car"))
})

test_that("NORB downloader assembles requested splits from local fixtures", {
  tmpdir <- tempfile()
  dir.create(tmpdir)

  training_base <- "smallnorb-5x46789x9x18x6x2x96x96-training"
  testing_base <- "smallnorb-5x01235x9x18x6x2x96x96-testing"

  write_gz_norb_images(file.path(tmpdir, paste0(training_base, "-dat.mat.gz")))
  write_gz_norb_categories(
    file.path(tmpdir, paste0(training_base, "-cat.mat.gz")),
    labels = c(0, 4)
  )
  write_gz_norb_info(file.path(tmpdir, paste0(training_base, "-info.mat.gz")))

  write_gz_norb_images(file.path(tmpdir, paste0(testing_base, "-dat.mat.gz")))
  write_gz_norb_categories(
    file.path(tmpdir, paste0(testing_base, "-cat.mat.gz")),
    labels = c(2, 3)
  )
  write_gz_norb_info(file.path(tmpdir, paste0(testing_base, "-info.mat.gz")))

  training <- download_norb_small(
    base_url = file_base_url(tmpdir),
    split = "training",
    as = "matrix"
  )
  all <- download_norb_small(
    base_url = file_base_url(tmpdir),
    split = "all",
    as = "matrix"
  )

  expect_equal(dim(training$data), c(2L, 4L))
  expect_equal(as.character(training$meta$Split), c("training", "training"))
  expect_equal(as.character(training$meta$Description), c("Animal", "Car"))

  expect_equal(dim(all$data), c(4L, 4L))
  expect_equal(
    as.character(all$meta$Split),
    c("training", "training", "testing", "testing")
  )
  expect_equal(
    as.character(all$meta$Description),
    c("Animal", "Car", "Airplane", "Truck")
  )
})

test_that("20 Newsgroups downloader extracts a local tarball and removes it", {
  source_root <- tempfile()
  train_dir <- file.path(source_root, "20news-bydate-train", "alt.atheism")
  dir.create(train_dir, recursive = TRUE)
  writeLines(c("Header: value", "", "Body"), file.path(train_dir, "1"))

  tarball <- tempfile(fileext = ".tar.gz")
  oldwd <- setwd(source_root)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(
    tarball,
    files = "20news-bydate-train",
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
  expect_length(list.files(outdir, pattern = "\\.tar\\.gz$"), 0)
})
