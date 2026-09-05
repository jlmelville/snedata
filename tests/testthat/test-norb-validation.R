test_that("NORB rejects colliding decimal signatures", {
  # Both byte sequences previously collapsed to the string 85766130.
  for (bytes in list(c(85L, 76L, 61L, 30L), c(8L, 57L, 66L, 130L))) {
    con <- rawConnection(as.raw(c(bytes, 4L, 0L, 0L, 0L)), "rb")
    if (bytes[[1]] == 85L) {
      expect_silent(snedata:::read_norb_header(con, "byte", 4L, "fixture"))
    } else {
      expect_error(
        snedata:::read_norb_header(con, "byte", 4L, "fixture"),
        "invalid matrix magic"
      )
    }
    close(con)
  }
})

test_that("NORB formatter enforces split-specific instances and unique tuples", {
  images <- matrix(0L, nrow = 2L, ncol = 18432L)
  for (split in c("training", "testing")) {
    valid <- if (split == "training") 4L else 0L
    invalid <- if (split == "training") 0L else 4L
    info <- matrix(rep(c(valid, 0L, 0L, 0L), 2L), nrow = 4L)
    expect_error(
      snedata:::format_norb_result(images, info, c(0L, 0L), split),
      "unique observation tuples"
    )
    # Category is part of the identity: different categories may share poses.
    expect_silent(snedata:::format_norb_result(
      images,
      info,
      c(0L, 1L),
      split,
      as = "list"
    ))
    info[1L, 1L] <- invalid
    expect_error(
      snedata:::format_norb_result(images, info, c(0L, 1L), split),
      "field 'instance'"
    )
  }
})

test_that("NORB complete metadata grids cannot hide duplicated observations", {
  # Check metadata directly to avoid allocating 448 million image pixels.
  for (split in c("training", "testing")) {
    grid <- expand.grid(
      category = 0:4,
      instance = if (split == "training") c(4L, 6:9) else c(0:3, 5L),
      elevation = 0:8,
      azimuth = seq(0L, 34L, 2L),
      lighting = 0:5
    )
    info <- t(as.matrix(grid[-1L]))
    expect_silent(snedata:::validate_norb_metadata(
      info,
      grid$category,
      split,
      expected_count = 24300L
    ))
    expect_error(
      snedata:::validate_norb_metadata(
        info[, -1L],
        grid$category[-1L],
        split,
        expected_count = 24300L
      ),
      "row_count"
    )
    info[, 1L] <- info[, 2L]
    grid$category[1L] <- grid$category[2L]
    expect_error(
      snedata:::validate_norb_metadata(
        info,
        grid$category,
        split,
        expected_count = 24300L
      ),
      "unique observation tuples"
    )
  }
})
