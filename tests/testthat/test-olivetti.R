context("Olivetti faces")

test_that("show_olivetti_face rejects out-of-range face indices", {
  expect_error(
    show_olivetti_face(data.frame(), face = 41, pose = 1),
    "face must be an integer between 1 and 40"
  )
})
