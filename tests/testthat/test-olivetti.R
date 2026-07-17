test_that("show_olivetti_face rejects out-of-range face indices", {
  expect_error(
    show_olivetti_face(data.frame(), face = 41, pose = 1),
    "face must be an integer between 1 and 40"
  )
})

test_that("face loaders do not attach RnavGraphImageData", {
  skip_if_not_installed("RnavGraphImageData")
  skip_if(
    "package:RnavGraphImageData" %in% search(),
    "RnavGraphImageData is already attached"
  )

  frey <- frey_faces()
  olivetti <- olivetti_faces()

  expect_equal(dim(frey), c(1965L, 561L))
  expect_equal(dim(olivetti), c(400L, 4097L))
  expect_false("package:RnavGraphImageData" %in% search())
})
