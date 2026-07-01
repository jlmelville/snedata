context("simulation shapes")

test_that("random_walk returns the requested shape", {
  set.seed(42)
  df <- random_walk(n = 5, dim = 3)

  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("X1", "X2", "X3", "color"))
})

test_that("random_jump returns the requested shape", {
  set.seed(42)
  df <- random_jump(n = 5, dim = 3)

  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("X1", "X2", "X3", "color"))
})
