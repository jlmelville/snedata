test_that("matrix memory estimates use the requested storage size", {
  expect_equal(estimate_matrix_bytes(2, 3, "integer"), 24)
  expect_equal(estimate_matrix_bytes(2, 3, "double"), 48)
  expect_equal(format_memory_bytes(1024^3), "1.00 GiB")
  expect_error(estimate_matrix_bytes(-1, 3), "non-negative finite integers")
})

test_that("wide data-frame warnings report the estimated pixel allocation", {
  expect_warning(
    warn_wide_data_frame(
      "fixture",
      n_rows = 2,
      n_cols = 3,
      storage = "integer",
      as = "data.frame",
      threshold = 1
    ),
    "fixture.*24.00 B.*as = \"matrix\""
  )
  expect_no_warning(
    warn_wide_data_frame(
      "fixture",
      n_rows = 2,
      n_cols = 3,
      storage = "integer",
      as = "matrix",
      threshold = 1
    )
  )
})

test_that("large downloaders warn before starting acquisition", {
  with_mocked_bindings(
    download_asset = function(...) stop("stop before download"),
    expect_warning(
      expect_error(download_cifar10(as = "data.frame"), "stop before download"),
      "CIFAR-10.*wide data frame"
    ),
    .package = "snedata"
  )
  with_mocked_bindings(
    read_norb_all_data = function(...) stop("stop before download"),
    expect_warning(
      expect_error(
        download_norb_small(split = "all", as = "data.frame"),
        "stop before download"
      ),
      "Small NORB.*wide data frame"
    ),
    .package = "snedata"
  )
  with_mocked_bindings(
    download_asset = function(...) stop("stop before download"),
    expect_warning(
      expect_error(
        snedata:::download_coil(
          url = "https://example.test/coil.zip",
          as = "data.frame",
          spec = snedata:::coil100_spec()
        ),
        "stop before download"
      ),
      "COIL-100.*wide data frame"
    ),
    .package = "snedata"
  )
})
