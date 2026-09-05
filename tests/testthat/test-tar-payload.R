write_tar_payload_fixture <- function(
  path,
  size,
  stored = size,
  padding = TRUE,
  next_type = "0"
) {
  header <- raw(512L)
  header[1:4] <- charToRaw("data")
  header[125:136] <- c(charToRaw(sprintf("%011o", as.integer(size))), as.raw(0))
  header[157L] <- charToRaw("0")
  con <- gzfile(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(header, con)
  writeBin(rep(as.raw(42), stored), con)
  if (padding) {
    writeBin(raw((512L - size %% 512L) %% 512L), con)
    header[125:136] <- c(charToRaw("00000000000"), as.raw(0))
    header[157L] <- charToRaw(next_type)
    writeBin(header, con)
    writeBin(raw(1024L), con)
  }
}

test_that("TAR payload consumption preserves padding and following headers", {
  # The private validator owns pre-extraction type and truncation safety.
  path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(path), add = TRUE)
  for (size in c(0L, 512L, 513L, 2L * 1024L^2L + 3L)) {
    write_tar_payload_fixture(path, size)
    expect_silent(snedata:::validate_tar_entry_types(path, "fixture"))
    write_tar_payload_fixture(path, size, next_type = "2")
    expect_error(
      snedata:::validate_tar_entry_types(path, "fixture"),
      "unsafe tar link entry"
    )
  }
})

test_that("TAR validation rejects missing payload bytes and final padding", {
  path <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(path), add = TRUE)
  size <- 2L * 1024L^2L + 3L
  for (stored in c(512L, size - 1L, size)) {
    write_tar_payload_fixture(path, size, stored = stored, padding = FALSE)
    expect_error(
      snedata:::validate_tar_entry_types(path, "fixture"),
      "truncated tar payload"
    )
  }
})
