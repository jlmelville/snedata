test_that("sphere sampling is isotropic with unit radius", {
  set.seed(42)
  xyz <- as.matrix(sphere(10000L)[1:3])
  expect_equal(rowSums(xyz^2), rep(1, nrow(xyz)), tolerance = 1e-12)
  # Loose moment bounds protect the construction, not RNG conformance.
  expect_true(all(abs(colMeans(xyz)) < 0.03))
  expect_true(all(abs(colMeans(xyz^2) - 1 / 3) < 0.03))
})

test_that("ball sampling has uniform radial volume and isotropic directions", {
  set.seed(42)
  for (d in c(1L, 3L, 7L)) {
    xyz <- as.matrix(ball(10000L, rad = 2, ndim = d)[seq_len(d)]) / 2
    radius <- sqrt(rowSums(xyz^2))
    volume <- radius^d
    expect_true(all(radius <= 1))
    expect_lt(abs(mean(volume) - 0.5), 0.03)
    expect_lt(abs(mean(volume^2) - 1 / 3), 0.03)
    expect_true(all(abs(colMeans(xyz)) < 0.03))
    expect_true(all(abs(colMeans(xyz^2) - 1 / (d + 2)) < 0.03))
  }
})

test_that("random walk accumulates supplied increments", {
  draws <- list(c(1, 2), c(3, 4), c(-1, 1))
  i <- 0L
  local_mocked_bindings(
    rnorm = function(n) {
      i <<- i + 1L
      stopifnot(n == 2L)
      draws[[i]]
    },
    .package = "stats"
  )
  result <- random_walk(3L, 2L)
  expect_equal(
    unname(as.matrix(result[1:2])),
    matrix(c(1, 2, 4, 6, 3, 7), nrow = 3L, byrow = TRUE)
  )
})

test_that("random jump adds scaled noise without feeding it into the walk", {
  draws <- c(1, 10, 2, -1, -1, 3)
  i <- 0L
  local_mocked_bindings(
    rnorm = function(n) {
      i <<- i + 1L
      rep(draws[[i]], n)
    },
    .package = "stats"
  )
  result <- random_jump(3L, 4L)
  expect_equal(
    unname(as.matrix(result[1:4])),
    matrix(rep(c(21, 1, 8), each = 4L), nrow = 3L, byrow = TRUE)
  )
})
