file_base_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE), "/")
}

write_gz_mnist_images <- function(
  path,
  n_images = 2,
  n_rows = 2,
  n_cols = 2,
  pixels = seq_len(n_images * n_rows * n_cols)
) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(2051), f, size = 4, endian = "big")
  writeBin(as.integer(n_images), f, size = 4, endian = "big")
  writeBin(as.integer(n_rows), f, size = 4, endian = "big")
  writeBin(as.integer(n_cols), f, size = 4, endian = "big")
  writeBin(as.integer(pixels), f, size = 1)
}

write_gz_mnist_labels <- function(path, labels = c(0, 5, 9)) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(2049), f, size = 4, endian = "big")
  writeBin(as.integer(length(labels)), f, size = 4, endian = "big")
  writeBin(as.integer(labels), f, size = 1)
}

write_gz_qmnist_labels <- function(path, magic = 3074, labels = c(7, 4)) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(magic), f, size = 4, endian = "big")
  writeBin(as.integer(length(labels)), f, size = 4, endian = "big")
  writeBin(as.integer(3), f, size = 4, endian = "big")
  label_rows <- unlist(lapply(labels, function(label) c(label, 0, 0)))
  writeBin(as.integer(label_rows), f, size = 4, endian = "big")
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

test_that("QMNIST extended label parser reads labels and reports magic 3074", {
  tmpdir <- tempdir()
  write_gz_qmnist_labels(file.path(tmpdir, "labels.gz"))
  write_gz_qmnist_labels(file.path(tmpdir, "bad-labels.gz"), magic = 2049)

  labels <- snedata:::parse_extended_label_file(
    "labels.gz",
    base_url = file_base_url(tmpdir)
  )

  expect_equal(labels, c(7, 4))
  expect_error(
    snedata:::parse_extended_label_file(
      "bad-labels.gz",
      base_url = file_base_url(tmpdir)
    ),
    "magic number 3074"
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
    file.path(tmpdir, "qmnist-train-labels-idx2-int.gz"),
    labels = c(7, 4)
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
