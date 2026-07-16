expect_hex_colors <- function(x) {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", x)))
}

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

test_that("synthetic_hierarchical_data returns the requested shape", {
  skip_if_not_installed("colorspace")

  set.seed(42)
  df <- synthetic_hierarchical_data(n = 2, dim = 3)

  expect_equal(nrow(df), 250)
  expect_equal(ncol(df), 10)
  expect_equal(
    names(df),
    c(
      "X1",
      "X2",
      "X3",
      "macro_label",
      "meso_label",
      "micro_label",
      "color",
      "macro_color",
      "meso_color",
      "micro_color"
    )
  )
})

test_that("synthetic_hierarchical_data labels and colors the hierarchy", {
  skip_if_not_installed("colorspace")

  set.seed(42)
  df <- synthetic_hierarchical_data(n = 2, dim = 3)

  expect_equal(as.character(head(df$micro_label, 2)), rep("0_0_0", 2))
  expect_equal(as.character(tail(df$micro_label, 2)), rep("4_4_4", 2))
  expect_equal(as.integer(table(df$micro_label)), rep(2L, 125))
  expect_equal(levels(df$macro_label), as.character(0:4))
  expect_equal(length(levels(df$meso_label)), 25)
  expect_equal(length(levels(df$micro_label)), 125)

  expect_true(all(df$color == df$meso_color))
  expect_false(anyNA(df$macro_color))
  expect_false(anyNA(df$meso_color))
  expect_false(anyNA(df$micro_color))

  color_maps <- synthetic_hierarchical_color_maps(
    n_macro = 5,
    n_meso = 5,
    n_micro = 5,
    anchors = c("#377EB8", "#E41A1C", "#4DAF4A", "#A65628", "#999999")
  )
  expect_equal(
    unname(color_maps$macro),
    c(
      "#377EB8",
      "#E41A1C",
      "#4DAF4A",
      "#A65628",
      "#999999"
    )
  )
  expect_equal(
    unique(df$macro_color[df$macro_label == "0"]),
    unname(color_maps$macro["0"])
  )
  expect_equal(
    unique(df$meso_color[df$meso_label == "0_0"]),
    unname(color_maps$meso["0_0"])
  )
  expect_equal(
    unique(df$micro_color[df$micro_label == "4_4_4"]),
    unname(color_maps$micro["4_4_4"])
  )
})

test_that("synthetic_hierarchical_color_maps builds hierarchy maps", {
  skip_if_not_installed("colorspace")

  maps <- synthetic_hierarchical_color_maps(
    n_macro = 2,
    n_meso = 3,
    n_micro = 4,
    anchors = c("#E41A1C", "#377EB8")
  )

  expect_equal(names(maps$macro), c("0", "1"))
  expect_equal(
    names(maps$meso),
    c("0_0", "0_1", "0_2", "1_0", "1_1", "1_2")
  )
  expect_equal(length(maps$micro), 24)
  expect_hex_colors(maps$macro)
  expect_hex_colors(maps$meso)
  expect_hex_colors(maps$micro)
  expect_equal(unname(maps$macro), c("#E41A1C", "#377EB8"))
  expect_equal(unname(maps$meso["1_2"]), unname(maps$micro["1_2_2"]))
})

test_that("grouped_palette creates grouped blocks from supplied anchors", {
  skip_if_not_installed("colorspace")

  # Internal helper used to experiment with replacing the hard-coded hierarchy.
  pal <- grouped_palette(
    c(5, 5, 5),
    anchors = c("#E41A1C", "#377EB8", "#999999")
  )
  swapped <- grouped_palette(
    c(5, 5, 5),
    anchors = c("#377EB8", "#E41A1C", "#999999")
  )
  gray_rgb <- grDevices::col2rgb(pal[11:15])

  expect_equal(length(pal), 15)
  expect_hex_colors(pal)
  expect_equal(length(unique(pal)), 15)
  expect_false(identical(pal, swapped))
  expect_true(all(gray_rgb["red", ] == gray_rgb["green", ]))
  expect_true(all(gray_rgb["green", ] == gray_rgb["blue", ]))
})

test_that("synthetic_hierarchical_data is reproducible with set.seed", {
  skip_if_not_installed("colorspace")

  set.seed(42)
  df1 <- synthetic_hierarchical_data(n = 1, dim = 2)
  set.seed(42)
  df2 <- synthetic_hierarchical_data(n = 1, dim = 2)

  expect_equal(df1, df2)
})

test_that("synthetic_hierarchical_data validates dimensions", {
  expect_error(
    synthetic_hierarchical_data(n = 0),
    "n must be a positive integer"
  )
  expect_error(
    synthetic_hierarchical_data(dim = 0),
    "dim must be a positive integer"
  )
  expect_error(
    synthetic_hierarchical_data(colors = "detail"),
    "should be one of"
  )
})

test_that("synthetic_hierarchical_data has dependency-free color options", {
  with_mocked_bindings(
    synthetic_hierarchical_color_maps = function(...) {
      stop("colorspace should not be used")
    },
    {
      set.seed(42)
      none <- synthetic_hierarchical_data(n = 2, dim = 3, colors = "none")
      expect_equal(dim(none), c(250L, 6L))
      expect_equal(
        names(none),
        c("X1", "X2", "X3", "macro_label", "meso_label", "micro_label")
      )
      expect_equal(as.integer(table(none$micro_label)), rep(2L, 125L))

      set.seed(42)
      macro <- synthetic_hierarchical_data(n = 2, dim = 3, colors = "macro")
      expect_equal(dim(macro), c(250L, 8L))
      expect_equal(
        names(macro),
        c(
          "X1",
          "X2",
          "X3",
          "macro_label",
          "meso_label",
          "micro_label",
          "color",
          "macro_color"
        )
      )
      expect_identical(macro$color, macro$macro_color)
      expect_equal(
        as.vector(tapply(macro$macro_color, macro$macro_label, unique)),
        c("#377EB8", "#E41A1C", "#4DAF4A", "#A65628", "#999999")
      )
    },
    .package = "snedata"
  )
})

test_that("sphere and ball respect radius invariants", {
  set.seed(42)
  sphere_df <- sphere(n = 25)
  sphere_radius <- sqrt(rowSums(as.matrix(sphere_df[c("x", "y", "z")])^2))

  expect_equal(names(sphere_df), c("x", "y", "z", "color"))
  expect_equal(nrow(sphere_df), 25)
  expect_equal(sphere_radius, rep(1, 25), tolerance = 1e-12)
  expect_hex_colors(sphere_df$color)

  set.seed(42)
  ball_df <- ball(n = 25, rad = 2, ndim = 3)
  ball_radius <- sqrt(rowSums(as.matrix(ball_df[c("x", "y", "z")])^2))

  expect_equal(names(ball_df), c("x", "y", "z", "color"))
  expect_true(all(ball_radius <= 2 + 1e-12))
  expect_hex_colors(ball_df$color)

  set.seed(42)
  ball4_df <- ball(n = 5, rad = 1, ndim = 4)

  expect_equal(dim(ball4_df), c(5L, 5L))
  expect_equal(names(ball4_df), c(paste0("X", 1:4), "color"))
})

test_that("helix points lie on the configured torus without duplicate endpoints", {
  df <- helix(n = 20, rmajor = 2, rminor = 1, nwinds = 3)
  radial <- sqrt(df$x^2 + df$y^2)

  expect_equal(names(df), c("x", "y", "z", "color"))
  expect_equal((radial - 2)^2 + df$z^2, rep(1, 20), tolerance = 1e-12)
  expect_gt(
    sqrt(sum((as.matrix(df[1, 1:3]) - as.matrix(df[20, 1:3]))^2)),
    1e-10
  )
  expect_hex_colors(df$color)
})

test_that("Swiss roll and S-curve variants preserve geometric constraints", {
  set.seed(42)
  swiss <- swiss_roll(n = 30, min_phi = pi, max_phi = 2 * pi, max_z = 3)
  swiss_radius <- sqrt(swiss$x^2 + swiss$y^2)

  expect_equal(names(swiss), c("x", "y", "z", "color"))
  expect_true(all(swiss_radius >= pi))
  expect_true(all(swiss_radius <= 2 * pi))
  expect_true(all(swiss$z >= 0 & swiss$z <= 3))
  expect_hex_colors(swiss$color)

  set.seed(42)
  hole <- s_curve_hole(n_samples = 100, noise = 0)
  squared_distance_from_hole <- rowSums(
    sweep(as.matrix(hole[c("x", "y", "z")]), 2, c(0, 1, 0), `-`)^2
  )

  expect_equal(names(hole), c("x", "y", "z", "color"))
  expect_true(nrow(hole) <= 100)
  expect_true(all(squared_distance_from_hole > 0.3))
  expect_hex_colors(hole$color)
})

test_that("curve2d returns the documented sampled x range", {
  set.seed(42)
  df <- curve2d()

  expect_equal(names(df), c("x", "y", "color"))
  expect_equal(nrow(df), 1450)
  expect_equal(df$x[[1]], -5.5)
  expect_equal(df$x[[nrow(df)]], 8.99)
  expect_hex_colors(df$color)
})

test_that("taspheres labels small spheres and the enclosing sphere", {
  set.seed(42)
  df <- taspheres(n_samples = 2, d = 3, n_spheres = 4, r = 2)
  big_sphere_radius <- sqrt(rowSums(as.matrix(df[7:26, 1:4])^2))

  expect_equal(dim(df), c(26L, 5L))
  expect_equal(names(df), c(paste0("X", 1:4), "labels"))
  expect_equal(as.integer(table(df$labels)), c(2L, 2L, 2L, 20L))
  expect_equal(unname(big_sphere_radius), rep(10, 20), tolerance = 1e-10)
})

test_that("taspheres validates its scalar parameters", {
  expect_error(taspheres(n_samples = 0), "n_samples must be a positive integer")
  expect_error(taspheres(d = Inf), "d must be a positive integer")
  expect_error(
    taspheres(n_spheres = 1),
    "n_spheres must be an integer greater than or equal to 2"
  )
  expect_error(taspheres(r = 0), "r must be a positive finite numeric scalar")
  expect_error(taspheres(r = Inf), "r must be a positive finite numeric scalar")
})

test_that("deterministic Distill-style generators return expected structures", {
  grid <- grid_data(n = 3)
  circle <- circle_data(n = 4)
  ortho <- ortho_curve(n = 4)
  simplex <- simplex_data(n = 3, noise = 0, color = "#112233")

  expect_equal(names(grid), c("x", "y", "color"))
  expect_equal(nrow(grid), 9)
  expect_equal(grid$x, c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L))
  expect_equal(grid$y, c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L))
  expect_hex_colors(grid$color)

  expect_equal(circle$x, c(1, 0, -1, 0), tolerance = 1e-12)
  expect_equal(circle$y, c(0, 1, 0, -1), tolerance = 1e-12)
  expect_hex_colors(circle$color)

  # fmt: skip
  expect_equal(
    unname(as.matrix(ortho[1:4])),
    matrix(
      c(
        0, 0, 0, 0,
        1, 0, 0, 0,
        1, 1, 0, 0,
        1, 1, 1, 0
      ),
      nrow = 4,
      byrow = TRUE
    )
  )
  expect_hex_colors(ortho$color)

  expect_equal(unname(as.matrix(simplex[1:3])), diag(3))
  expect_equal(simplex$color, rep("#112233", 3))
})

test_that("linked and unlinked rings differ by their x-offset", {
  linked <- link_data(n = 4)
  unlinked <- unlink_data(n = 4)

  expect_equal(dim(linked), c(8L, 4L))
  expect_equal(dim(unlinked), c(8L, 4L))
  expect_equal(mean(linked$x[5:8]) - mean(linked$x[1:4]), 1)
  expect_equal(mean(unlinked$x[5:8]) - mean(unlinked$x[1:4]), 3)
  expect_equal(as.integer(table(linked$color)), c(4L, 4L))
  expect_equal(as.integer(table(unlinked$color)), c(4L, 4L))
})

test_that("simulation generators reject pathological scalar inputs", {
  cases <- list(
    list(sphere, list(n = 0), "n must be a positive integer"),
    list(ball, list(rad = Inf), "rad must be a positive finite numeric scalar"),
    list(
      helix,
      list(nwinds = 0),
      "nwinds must be a positive finite numeric scalar"
    ),
    list(
      swiss_roll,
      list(max_phi = pi),
      "max_phi must be greater than min_phi"
    ),
    list(
      s_curve,
      list(noise = -1),
      "noise must be a nonnegative finite numeric scalar"
    ),
    list(
      s_curve_hole,
      list(n_samples = 0),
      "n_samples must be a positive integer"
    ),
    list(grid_data, list(n = 0), "n must be a positive integer"),
    list(
      gaussian_data,
      list(n = 1, dim = Inf),
      "dim must be a positive integer"
    ),
    list(
      long_gaussian_data,
      list(n = 1, dim = 0),
      "dim must be a positive integer"
    ),
    list(circle_data, list(n = 0), "n must be a positive integer"),
    list(random_circle_data, list(n = 0), "n must be a positive integer"),
    list(
      random_circle_cluster_data,
      list(n = 0),
      "n must be a positive integer"
    ),
    list(
      two_clusters_data,
      list(n = 1, dim = 1:3),
      "dim must be a positive integer vector of length 1 or 2"
    ),
    list(
      two_different_clusters_data,
      list(n = 1, scale = 0),
      "scale must be a positive finite numeric scalar"
    ),
    list(three_clusters_data, list(n = 0), "n must be a positive integer"),
    list(
      subset_clusters_data,
      list(n = 1, big_sdev = Inf),
      "big_sdev must be a positive finite numeric scalar"
    ),
    list(
      simplex_data,
      list(n = 1, noise = NaN),
      "noise must be a nonnegative finite numeric scalar"
    ),
    list(cube_data, list(n = 1, dim = 0), "dim must be a positive integer"),
    list(unlink_data, list(n = 0), "n must be a positive integer"),
    list(link_data, list(n = 0), "n must be a positive integer"),
    list(trefoil_data, list(n = 0), "n must be a positive integer"),
    list(long_cluster_data, list(n = 0), "n must be a positive integer"),
    list(ortho_curve, list(n = 0), "n must be a positive integer"),
    list(random_walk, list(n = 0, dim = 1), "n must be a positive integer"),
    list(random_jump, list(n = 1, dim = 0), "dim must be a positive integer")
  )

  for (case in cases) {
    expect_error(do.call(case[[1]], case[[2]]), case[[3]])
  }
})

test_that("two_clusters_data accepts one or two positive dimensions", {
  df <- two_clusters_data(n = 2, dim = c(2, 3))

  expect_equal(dim(df), c(4L, 4L))
  expect_equal(names(df), c("X1", "X2", "X3", "color"))
  expect_equal(df$X3[1:2], c(0, 0))
})
