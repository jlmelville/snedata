test_that("is_installed returns requireNamespace status", {
  expect_true(snedata:::is_installed("stats"))
  expect_false(snedata:::is_installed("snedata-package-that-does-not-exist"))
})

test_that("tsmessage uses explicit verbosity and preserves forced messages", {
  expect_message(
    snedata:::tsmessage("visible", verbose = TRUE, time_stamp = FALSE),
    "visible"
  )
  expect_message(
    snedata:::tsmessage("quiet", verbose = FALSE, time_stamp = FALSE),
    NA
  )
  expect_message(
    snedata:::tsmessage("forced", force = TRUE, time_stamp = FALSE),
    "forced"
  )
})
