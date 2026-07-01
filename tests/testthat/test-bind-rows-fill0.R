context("row binding with zero-filled dimensions")

test_that("two_clusters_data preserves extra dimensions with zeros", {
  set.seed(42)
  df <- two_clusters_data(n = 3, dim = c(2, 4))

  expect_equal(nrow(df), 6)
  expect_equal(names(df), c("X1", "X2", "X3", "X4", "color"))
  expect_true(all(df[1:3, c("X3", "X4")] == 0))
})
