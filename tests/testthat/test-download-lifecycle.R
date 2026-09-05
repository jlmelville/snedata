archive_downloaders <- function() {
  list(
    cifar = list(
      fun = download_cifar10,
      argument = "destfile",
      ext = ".tar.gz"
    ),
    coil20 = list(fun = download_coil20, argument = "file", ext = ".zip"),
    coil100 = list(fun = download_coil100, argument = "file", ext = ".zip")
  )
}

test_that("archive downloaders preserve existing final destinations", {
  local_mocked_bindings(
    stop_if_not_installed = function(...) NULL,
    download_asset = function(...) stop("download must not start"),
    .package = "snedata"
  )
  parent <- tempfile()
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  bytes <- as.raw(c(0, 17, 255, 10))

  for (case in archive_downloaders()) {
    for (kind in c("file", "directory")) {
      for (extension in c(FALSE, TRUE)) {
        input <- tempfile(tmpdir = parent)
        final <- paste0(input, case$ext)
        if (kind == "directory") {
          dir.create(final)
          sentinel <- file.path(final, "keep.bin")
        } else {
          sentinel <- final
        }
        writeBin(bytes, sentinel)
        args <- list(timeout = 0)
        args[[case$argument]] <- if (extension) final else input
        expect_error(do.call(case$fun, args), "already exists")
        expect_identical(readBin(sentinel, "raw", 100L), bytes)
      }
    }
  }
})

test_that("partial archives follow cleanup and leave caller siblings intact", {
  local_mocked_bindings(
    stop_if_not_installed = function(...) NULL,
    download_asset = function(url, destfile, ...) {
      writeBin(as.raw(c(1, 2, 3)), destfile)
      stop("partial download failed")
    },
    .package = "snedata"
  )
  parent <- tempfile()
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  sibling <- file.path(parent, "caller-directory")
  dir.create(sibling)
  writeLines("keep", file.path(sibling, "sentinel"))
  writeLines("keep file", file.path(parent, "caller-file"))

  for (case in archive_downloaders()) {
    for (cleanup in c(TRUE, FALSE)) {
      input <- tempfile(tmpdir = parent)
      args <- list(cleanup = cleanup)
      args[[case$argument]] <- input
      expect_error(do.call(case$fun, args), "partial download failed")
      final <- paste0(input, case$ext)
      expect_identical(file.exists(final), !cleanup)
      if (!cleanup) {
        expect_identical(readBin(final, "raw", 100L), as.raw(c(1, 2, 3)))
      }
      expect_identical(readLines(file.path(sibling, "sentinel")), "keep")
      expect_identical(readLines(file.path(parent, "caller-file")), "keep file")
    }
  }
})

test_that("Newsgroups retains downloaded archives on extraction failure when requested", {
  local_mocked_bindings(
    download_asset = function(url, destfile, ...)
      writeBin(as.raw(1:3), destfile),
    extract_tar_safely = function(...) stop("extraction failed"),
    .package = "snedata"
  )
  for (cleanup in c(TRUE, FALSE)) {
    parent <- tempfile()
    dir.create(parent)
    on.exit(unlink(parent, recursive = TRUE), add = TRUE)
    writeLines("keep", file.path(parent, "sentinel"))
    expect_error(
      download_twenty_newsgroups(tmpdir = parent, cleanup = cleanup),
      "extraction failed"
    )
    archives <- list.files(
      parent,
      pattern = "tar.gz$",
      recursive = TRUE,
      full.names = TRUE
    )
    expect_length(archives, if (cleanup) 0L else 1L)
    if (!cleanup) {
      expect_identical(readBin(archives, "raw", 100L), as.raw(1:3))
    }
    expect_identical(readLines(file.path(parent, "sentinel")), "keep")
  }
})
