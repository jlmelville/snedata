file_base_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE), "/")
}

write_gz_mnist_images <- function(path) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(2051), f, size = 4, endian = "big")
  writeBin(as.integer(2), f, size = 4, endian = "big")
  writeBin(as.integer(2), f, size = 4, endian = "big")
  writeBin(as.integer(2), f, size = 4, endian = "big")
  writeBin(as.integer(1:8), f, size = 1)
}

write_gz_mnist_labels <- function(path) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(2049), f, size = 4, endian = "big")
  writeBin(as.integer(3), f, size = 4, endian = "big")
  writeBin(as.integer(c(0, 5, 9)), f, size = 1)
}

write_gz_qmnist_labels <- function(path, magic = 3074) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  writeBin(as.integer(magic), f, size = 4, endian = "big")
  writeBin(as.integer(2), f, size = 4, endian = "big")
  writeBin(as.integer(3), f, size = 4, endian = "big")
  writeBin(as.integer(c(7, 100, 101, 4, 200, 201)), f,
    size = 4, endian = "big"
  )
}

write_norb_matrix_header <- function(f, type, ndim) {
  magic <- switch(type,
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

write_gz_norb_categories <- function(path) {
  f <- gzfile(path, "wb")
  on.exit(close(f), add = TRUE)

  write_norb_matrix_header(f, "integer", ndim = 1)
  writeBin(as.integer(c(3, 0, 0)), f, size = 4, endian = "little")
  writeBin(as.integer(c(0, 2, 4)), f, size = 4, endian = "little")
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

  images <- snedata:::parse_image_file("images.gz", base_url = file_base_url(tmpdir))
  labels <- snedata:::parse_label_file("labels.gz", base_url = file_base_url(tmpdir))

  expect_equal(dim(images), c(2L, 4L))
  expect_equal(images[1, ], 1:4)
  expect_equal(images[2, ], 5:8)
  expect_equal(labels, c(0L, 5L, 9L))
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

test_that("20 Newsgroups downloader extracts a local tarball and removes it", {
  source_root <- tempfile()
  train_dir <- file.path(source_root, "20news-bydate-train", "alt.atheism")
  dir.create(train_dir, recursive = TRUE)
  writeLines(c("Header: value", "", "Body"), file.path(train_dir, "1"))

  tarball <- tempfile(fileext = ".tar.gz")
  oldwd <- setwd(source_root)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(tarball,
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

  expect_true(file.exists(file.path(outdir, "20news-bydate-train", "alt.atheism", "1")))
  expect_length(list.files(outdir, pattern = "\\.tar\\.gz$"), 0)
})
