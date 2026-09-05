test_that("Isomap passes plotting controls and preserves pixel orientation", {
  observed <- NULL
  local_mocked_bindings(
    image = function(x, ...) observed <<- list(x = x, args = list(...)),
    .package = "graphics"
  )
  pixels <- numeric(4096L)
  pixels[c(1L, 2L, 65L, 4096L)] <- c(11, 22, 33, 44)
  df <- as.data.frame(matrix(pixels, nrow = 1L))
  show_isomap_face(df, 1L, axes = TRUE, xlab = "horizontal", ylab = "vertical")
  expect_identical(observed$args$axes, TRUE)
  expect_identical(observed$args$xlab, "horizontal")
  expect_identical(observed$args$ylab, "vertical")
  expected <- matrix(0, 64L, 64L)
  expected[1L, 64L] <- 11
  expected[1L, 63L] <- 22
  expected[2L, 64L] <- 33
  expected[64L, 1L] <- 44
  expect_identical(observed$x, expected)
  show_isomap_face(df, 1L)
  expect_identical(observed$args$axes, FALSE)
  expect_identical(observed$args$xlab, "")
})

test_that("MNIST preserves asymmetric pixels in both result formats", {
  observed <- NULL
  local_mocked_bindings(
    image = function(x, ...) observed <<- x,
    .package = "graphics"
  )
  pixels <- matrix(0, nrow = 1L, ncol = 784L)
  pixels[1L, c(1L, 2L, 29L, 784L)] <- c(11, 22, 33, 44)
  expected <- matrix(0, 28L, 28L)
  expected[1L, 28L] <- 11
  expected[2L, 28L] <- 22
  expected[1L, 27L] <- 33
  expected[28L, 1L] <- 44
  canonical <- snedata:::new_image_result(
    pixels,
    data.frame(id = 1L),
    c(height = 28L, width = 28L),
    "gray",
    list(dataset = "MNIST", url = "local")
  )
  for (df in list(as.data.frame(pixels), canonical)) {
    show_mnist_digit(df, 1L)
    expect_identical(observed, expected)
  }
})

test_that("CIFAR preserves channel order and asymmetric pixel positions", {
  observed <- NULL
  local_mocked_bindings(
    rasterImage = function(image, ...) observed <<- image,
    .package = "graphics"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(
    {
      grDevices::dev.off()
      unlink(path)
    },
    add = TRUE
  )
  pixels <- matrix(0, nrow = 1L, ncol = 3072L)
  pixels[1L, c(1L, 1026L, 2081L)] <- 255
  expected <- matrix("#000000", 32L, 32L)
  expected[1L, 1L] <- "#FF0000"
  expected[1L, 2L] <- "#00FF00"
  expected[2L, 1L] <- "#0000FF"
  canonical <- snedata:::format_cifar_result(pixels, 0L, as = "list")
  for (df in list(as.data.frame(pixels), canonical)) {
    show_cifar(df, 1L)
    expect_identical(observed, expected)
  }
})
