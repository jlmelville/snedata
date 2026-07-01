test_that("linear_map maps constant finite input to the midpoint", {
  expect_equal(snedata:::linear_map(rep(1, 3)), rep(0.5, 3))
  expect_equal(snedata:::linear_map(rep(1, 3), from = -2, to = 4), rep(1, 3))
})

test_that("linear_map rejects non-finite input clearly", {
  expect_error(snedata:::linear_map(c(1, NA)), "finite")
  expect_error(snedata:::linear_map(c(1, Inf)), "finite")
})
