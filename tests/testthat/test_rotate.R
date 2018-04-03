library("snedata")
context("rotate bug")

rot <- function(x, y, z) {
  res <- snedata:::rotate(list(x = x, y = y, z = z))
  res <- unlist(res)
  names(res) <- NULL
  res
}

expect_equal(rot(1, 2, 3), c(1, 3.010, 1.984), tol = 1e-3)
expect_equal(rot(1, 1, 1), c(1, 1.310, 0.532), tol = 1e-3)

