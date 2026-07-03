test_that("Isomap Swiss-roll formatter returns coordinates and colors", {
  mat <- list(
    X.data = matrix(1:6, nrow = 3),
    Y.data = matrix(c(10, 20, 30, 40), nrow = 2)
  )

  df <- snedata:::format_isomap_swiss_roll(mat)

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("x", "y", "z", "u", "v", "color"))
  expect_equal(dim(df), c(2L, 6L))
  expect_equal(unname(unlist(df[1, 1:5])), c(1, 2, 3, 10, 20))
  expect_equal(df$color, snedata:::linear_color_map(c(10, 30)))
})

test_that("Isomap formatters validate expected MAT components", {
  expect_error(
    snedata:::format_isomap_swiss_roll(list()),
    "component 'X.data'"
  )
  expect_error(
    snedata:::format_isomap_swiss_roll(list(
      X.data = matrix(1:6, nrow = 2),
      Y.data = matrix(1:4, nrow = 2)
    )),
    "must have 3 rows"
  )
  expect_error(
    snedata:::format_isomap_faces(list(
      images = matrix(1:4096, nrow = 4096),
      poses = matrix(1:4, nrow = 2)
    )),
    "different column counts"
  )
})

test_that("Isomap faces formatter returns pixel and pose columns", {
  mat <- list(
    images = matrix(seq_len(4096 * 2), nrow = 4096),
    poses = matrix(c(1, 2, 3, 4), nrow = 2)
  )

  df <- snedata:::format_isomap_faces(mat)

  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(2L, 4098L))
  expect_equal(names(df)[1:3], c("px1", "px2", "px3"))
  expect_equal(names(df)[4096:4098], c("px4096", "pose1", "pose2"))
  expect_equal(unname(unlist(df[1, c(1, 4096, 4097, 4098)])), c(1, 4096, 1, 2))
})

test_that("Isomap face display validates inputs and plots a face", {
  mat <- list(
    images = matrix(seq_len(4096 * 2), nrow = 4096),
    poses = matrix(c(1, 2, 3, 4), nrow = 2)
  )
  df <- snedata:::format_isomap_faces(mat)

  expect_error(show_isomap_face(df, 0), "between 1 and 2")
  expect_error(
    show_isomap_face(data.frame(x = 1), 1),
    "at least 4096"
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(show_isomap_face(df, 1))
})
