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
  expect_equal(unique(df$macro_color[df$macro_label == "0"]), "#d53244")
  expect_equal(unique(df$meso_color[df$meso_label == "0_0"]), "#6c0016")
  expect_equal(unique(df$micro_color[df$micro_label == "4_4_4"]), "#ebd7a1")
})

test_that("synthetic_hierarchical_data is reproducible with set.seed", {
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
})
