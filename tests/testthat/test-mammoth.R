test_that("Mammoth coordinates use canonical X, Y, Z order", {
  mammoth10k <- snedata:::format_mammoth_coordinates(
    list(c(1, 3, 2), c(4, 6, 5)),
    source_order = c("X", "Z", "Y"),
    expected_n = 2L
  )
  mammoth50k <- snedata:::format_mammoth_coordinates(
    list(c(2, 1, 3), c(5, 4, 6)),
    source_order = c("Y", "X", "Z"),
    expected_n = 2L
  )

  expect_equal(names(mammoth10k), c("X", "Y", "Z"))
  expect_equal(mammoth10k, mammoth50k)
  expect_equal(
    unname(as.matrix(mammoth10k)),
    matrix(c(1, 2, 3, 4, 5, 6), ncol = 3, byrow = TRUE)
  )
})

test_that("Mammoth coordinates reject malformed JSON structures", {
  valid_coordinates <- list(c(1, 3, 2), c(4, 6, 5))

  expect_error(
    snedata:::format_mammoth_coordinates(
      valid_coordinates,
      source_order = c("X", "Z", "Y"),
      expected_n = 3L
    ),
    "exactly 3 observations; found 2"
  )
  expect_error(
    snedata:::format_mammoth_coordinates(
      list(c(1, 2), c(4, 6, 5)),
      source_order = c("X", "Z", "Y"),
      expected_n = 2L
    ),
    "coordinate row 1 must contain exactly three finite numeric values"
  )
  expect_error(
    snedata:::format_mammoth_coordinates(
      list(c(1, Inf, 2), c(4, 6, 5)),
      source_order = c("X", "Z", "Y"),
      expected_n = 2L
    ),
    "coordinate row 1 must contain exactly three finite numeric values"
  )
  expect_error(
    snedata:::format_mammoth_coordinates(
      list(c(1, "two", 3), c(4, 6, 5)),
      source_order = c("X", "Z", "Y"),
      expected_n = 2L
    ),
    "coordinate row 1 must contain exactly three finite numeric values"
  )
  expect_error(
    snedata:::format_mammoth_coordinates(
      list(c(1, 3, 2), c(4, 6, 5)),
      source_order = c("X", "X", "Z"),
      expected_n = 2L
    ),
    "source_order must contain X, Y, and Z exactly once"
  )
})
