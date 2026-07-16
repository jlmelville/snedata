test_that("Mammoth coordinates use canonical X, Y, Z order", {
  mammoth10k <- snedata:::format_mammoth_coordinates(
    list(c(1, 3, 2), c(4, 6, 5)),
    source_order = c("X", "Z", "Y")
  )
  mammoth50k <- snedata:::format_mammoth_coordinates(
    list(c(2, 1, 3), c(5, 4, 6)),
    source_order = c("Y", "X", "Z")
  )

  expect_equal(names(mammoth10k), c("X", "Y", "Z"))
  expect_equal(mammoth10k, mammoth50k)
  expect_equal(
    unname(as.matrix(mammoth10k)),
    matrix(c(1, 2, 3, 4, 5, 6), ncol = 3, byrow = TRUE)
  )
})
