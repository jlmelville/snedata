test_that("Isomap Swiss-roll formatter returns coordinates and colors", {
  mat <- list(
    X.data = matrix(1:6, nrow = 3),
    Y.data = matrix(c(10, 20, 30, 40), nrow = 2)
  )

  df <- snedata:::format_isomap_swiss_roll(mat)

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("x", "y", "z", "u", "v", "color"))
  expect_equal(dim(df), c(2L, 6L))
  expect_equal(unname(unlist(df[1, 1:5])), c(1, 2, 3, 10, 20))
  expect_equal(df$color, snedata:::linear_color_map(c(10, 30)))
})

test_that("Isomap formatters validate expected MAT components", {
  expect_error(
    snedata:::format_isomap_swiss_roll(list()),
    "component 'X.data'"
  )
  expect_error(
    snedata:::format_isomap_swiss_roll(list(
      X.data = matrix(1:6, nrow = 2),
      Y.data = matrix(1:4, nrow = 2)
    )),
    "must have 3 rows"
  )
  expect_error(
    snedata:::format_isomap_faces(list(
      images = matrix(1:4096, nrow = 4096),
      poses = matrix(1:4, nrow = 2)
    )),
    "different column counts"
  )
})

test_that("Isomap faces formatter returns pixel and pose columns", {
  mat <- list(
    images = matrix(seq_len(4096 * 2), nrow = 4096),
    poses = matrix(c(1, 2, 3, 4), nrow = 2)
  )

  df <- snedata:::format_isomap_faces(mat)

  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(2L, 4098L))
  expect_equal(names(df)[1:3], c("px1", "px2", "px3"))
  expect_equal(names(df)[4096:4098], c("px4096", "pose1", "pose2"))
  expect_equal(unname(unlist(df[1, c(1, 4096, 4097, 4098)])), c(1, 4096, 1, 2))
})

test_that("Isomap face display validates inputs and plots a face", {
  mat <- list(
    images = matrix(seq_len(4096 * 2), nrow = 4096),
    poses = matrix(c(1, 2, 3, 4), nrow = 2)
  )
  df <- snedata:::format_isomap_faces(mat)

  expect_error(show_isomap_face(df, 0), "between 1 and 2")
  expect_error(
    show_isomap_face(data.frame(x = 1), 1),
    "at least 4096"
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(show_isomap_face(df, 1))
})

test_that("Isomap URL reader cleans downloaded and decompressed paths on errors", {
  paths <- new.env(parent = emptyenv())

  with_mocked_bindings(
    stop_if_not_installed = function(...) NULL,
    download_asset = function(url, destfile, verbose, timeout) {
      writeBin(as.raw(1), destfile)
      paths$downloaded <- destfile
    },
    decompress_isomap_compress = function(path, verbose) {
      out <- tempfile("isomap-decompressed-", fileext = ".mat")
      writeBin(as.raw(2), out)
      paths$decompressed <- out
      out
    },
    read_isomap_mat = function(path) {
      paths$read_path <- path
      stop("reader failed")
    },
    expect_error(
      snedata:::read_isomap_mat_url(
        "https://example.invalid/data.Z",
        ".Z",
        compression = "compress"
      ),
      "reader failed"
    ),
    .package = "snedata"
  )

  expect_equal(paths$read_path, paths$decompressed)
  expect_false(file.exists(paths$downloaded))
  expect_false(file.exists(paths$decompressed))
})

test_that("Isomap faces uses the mirror and preserves explicit argument behavior", {
  mirror_url <- paste0(
    "https://raw.githubusercontent.com/Mashimo/datascience/",
    "85cd93b9159edb906bf025eadd8aa1b648277ebc/datasets/face_data.mat"
  )
  archive_url <- paste0(
    "https://web.archive.org/web/20150922051706id_/",
    "http://isomap.stanford.edu/face_data.mat.Z"
  )
  observed <- list()
  local_mocked_bindings(
    read_isomap_mat_url = function(
      url,
      fileext,
      compression,
      verbose,
      timeout
    ) {
      observed[[length(observed) + 1L]] <<- list(
        url = url,
        fileext = fileext,
        compression = compression,
        verbose = verbose,
        timeout = timeout
      )
      list(images = matrix(1:4096, nrow = 4096), poses = matrix(1:2, nrow = 2))
    },
    .package = "snedata"
  )
  download_isomap_faces()
  download_isomap_faces("https://example.invalid/faces.Z", TRUE, 123)
  download_isomap_faces(compression = "compress")
  download_isomap_faces(compression = "none")
  download_isomap_faces(mirror_url)
  download_isomap_faces(verbose = TRUE, timeout = 123)
  expect_identical(
    observed[[1]],
    list(
      url = mirror_url,
      fileext = ".mat",
      compression = "none",
      verbose = FALSE,
      timeout = 1800
    )
  )
  expect_identical(
    observed[[2]],
    list(
      url = "https://example.invalid/faces.Z",
      fileext = ".mat.Z",
      compression = "compress",
      verbose = TRUE,
      timeout = 123
    )
  )
  expect_identical(
    observed[[3]],
    list(
      url = archive_url,
      fileext = ".mat.Z",
      compression = "compress",
      verbose = FALSE,
      timeout = 1800
    )
  )
  expect_identical(
    observed[[4]],
    list(
      url = archive_url,
      fileext = ".mat",
      compression = "none",
      verbose = FALSE,
      timeout = 1800
    )
  )
  expect_identical(
    observed[[5]],
    list(
      url = mirror_url,
      fileext = ".mat.Z",
      compression = "compress",
      verbose = FALSE,
      timeout = 1800
    )
  )
  expect_identical(
    observed[[6]],
    list(
      url = mirror_url,
      fileext = ".mat",
      compression = "none",
      verbose = TRUE,
      timeout = 123
    )
  )
})

test_that("Isomap faces default reads MAT data without a decompressor", {
  skip_if_not_installed("R.matlab")
  source <- tempfile(fileext = ".mat")
  on.exit(unlink(source), add = TRUE)
  images <- matrix(seq_len(4096 * 2), nrow = 4096)
  poses <- matrix(c(1, 2, 3, 4), nrow = 2)
  R.matlab::writeMat(source, images = images, poses = poses)
  downloaded <- NULL
  local_mocked_bindings(
    download_asset = function(url, destfile, verbose, timeout) {
      file.copy(source, destfile)
      downloaded <<- destfile
    },
    decompress_isomap_compress = function(...) stop("must not decompress"),
    .package = "snedata"
  )

  faces <- download_isomap_faces()

  expect_identical(names(faces), c(paste0("px", 1:4096), "pose1", "pose2"))
  expect_equal(as.matrix(faces[, 1:4096]), t(images), ignore_attr = TRUE)
  expect_equal(as.matrix(faces[, 4097:4098]), t(poses), ignore_attr = TRUE)
  expect_false(file.exists(downloaded))
  expect_true(file.exists(source))
})

test_that("Isomap faces default propagates download failures without retrying", {
  skip_if_not_installed("R.matlab")
  downloaded <- character()
  local_mocked_bindings(
    download_asset = function(url, destfile, verbose, timeout) {
      downloaded <<- c(downloaded, destfile)
      writeBin(as.raw(1), destfile)
      stop("download failed")
    },
    .package = "snedata"
  )

  expect_error(download_isomap_faces(), "download failed")

  expect_length(downloaded, 1L)
  expect_false(any(file.exists(downloaded)))
})

test_that("Isomap faces reads an uncompressed MAT file without a decompressor", {
  skip_if_not_installed("R.matlab")
  path <- tempfile(fileext = ".mat")
  on.exit(unlink(path), add = TRUE)
  images <- matrix(seq_len(4096 * 2), nrow = 4096)
  poses <- matrix(c(1, 2, 3, 4), nrow = 2)
  R.matlab::writeMat(path, images = images, poses = poses)
  local_mocked_bindings(
    decompress_isomap_compress = function(...) stop("must not decompress"),
    .package = "snedata"
  )
  faces <- download_isomap_faces(
    url = paste0("file://", normalizePath(path, winslash = "/")),
    compression = "none"
  )
  expect_identical(dim(faces), c(2L, 4098L))
  expect_identical(names(faces), c(paste0("px", 1:4096), "pose1", "pose2"))
  expect_equal(as.matrix(faces[, 1:4096]), t(images), ignore_attr = TRUE)
  expect_equal(as.matrix(faces[, 4097:4098]), t(poses), ignore_attr = TRUE)
})

test_that("Isomap faces cleans a malformed uncompressed download", {
  skip_if_not_installed("R.matlab")
  source <- tempfile(fileext = ".mat")
  on.exit(unlink(source), add = TRUE)
  R.matlab::writeMat(source, images = matrix(1:4096, nrow = 4096))
  paths <- new.env(parent = emptyenv())
  local_mocked_bindings(
    download_asset = function(url, destfile, verbose, timeout) {
      file.copy(source, destfile)
      paths$downloaded <- destfile
    },
    decompress_isomap_compress = function(...) stop("must not decompress"),
    .package = "snedata"
  )
  expect_error(
    download_isomap_faces(
      "https://example.invalid/faces.mat",
      compression = "none"
    ),
    "component 'poses'"
  )
  expect_false(file.exists(paths$downloaded))
  expect_true(file.exists(source))
})

test_that("Isomap faces rejects unsupported compression before downloading", {
  local_mocked_bindings(
    download_asset = function(...) stop("download must not start"),
    .package = "snedata"
  )
  expect_error(
    download_isomap_faces(compression = "gzip"),
    "'arg' should be one of"
  )
})
