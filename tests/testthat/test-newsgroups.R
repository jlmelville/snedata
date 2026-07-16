write_newsgroup_fixture <- function(root) {
  paths <- c(
    file.path(root, "20news-bydate-train", "alt.atheism"),
    file.path(root, "20news-bydate-train", "comp.graphics"),
    file.path(root, "20news-bydate-test", "alt.atheism"),
    file.path(root, "20news-bydate-test", "comp.graphics")
  )
  for (path in paths) {
    dir.create(path, recursive = TRUE)
  }

  writeLines(
    c("Subject: train atheist", "", "Body one"),
    file.path(paths[1], "1")
  )
  writeLines(
    c("Subject: train graphics", "", "Body two"),
    file.path(paths[2], "2")
  )
  writeLines(
    c("Subject: test atheist", "", "Body three"),
    file.path(paths[3], "3")
  )
  writeLines(
    c("Subject: test graphics", "", "Body four"),
    file.path(paths[4], "4")
  )
}

test_that("20 Newsgroups reader builds metadata from local folders", {
  root <- tempfile()
  write_newsgroup_fixture(root)

  df <- snedata:::read_newsgroups_data(root, subset = "all")

  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df),
    c("Id", "FileId", "Text", "Subset", "Label", "Newsgroup")
  )
  expect_equal(nrow(df), 4)
  expect_equal(as.character(df$Id), c("train_1", "train_2", "test_1", "test_2"))
  expect_equal(as.character(df$FileId), c("1", "2", "3", "4"))
  expect_equal(
    as.character(df$Subset),
    c("train", "train", "test", "test")
  )
  expect_equal(levels(df$Subset), c("train", "test"))
  expect_equal(
    as.character(df$Newsgroup),
    c("alt.atheism", "comp.graphics", "alt.atheism", "comp.graphics")
  )
  expect_equal(as.character(df$Label), c("0", "1", "0", "1"))
  expect_equal(df$Text[[1]], "Subject: train atheist\n\nBody one")
})

test_that("20 Newsgroups reader can read one subset", {
  root <- tempfile()
  write_newsgroup_fixture(root)

  df <- snedata:::read_newsgroups_data(root, subset = "test")

  expect_equal(nrow(df), 2)
  expect_equal(as.character(df$Id), c("test_1", "test_2"))
  expect_equal(as.character(df$Subset), c("test", "test"))
  expect_equal(as.character(df$Label), c("0", "1"))
})

test_that("setup_temp_directory reports whether it created the directory", {
  existing <- tempfile()
  dir.create(existing)
  missing <- tempfile()
  on.exit(unlink(c(existing, missing), recursive = TRUE), add = TRUE)

  existing_info <- snedata:::setup_temp_directory(existing)
  missing_info <- snedata:::setup_temp_directory(missing)

  expect_identical(
    existing_info,
    list(tmpdir = existing, created_tmpdir = FALSE)
  )
  expect_identical(missing_info, list(tmpdir = missing, created_tmpdir = TRUE))
  expect_true(dir.exists(missing))
})

test_that("newsgroups validates subset before creating a work directory", {
  tmpdir <- tempfile("newsgroups-parent-")

  expect_error(
    download_twenty_newsgroups(subset = "invalid", tmpdir = tmpdir),
    "should be one of"
  )
  expect_false(dir.exists(tmpdir))
})

test_that("newsgroups cleanup preserves caller-owned directories after errors", {
  tmpdir <- tempfile("newsgroups-parent-")
  dir.create(tmpdir)
  sentinel <- file.path(tmpdir, "caller-owned.txt")
  writeLines("keep", sentinel)

  with_mocked_bindings(
    download_twenty_newsgroups_data = function(...) stop("download failed"),
    expect_error(
      download_twenty_newsgroups(tmpdir = tmpdir, cleanup = TRUE),
      "download failed"
    ),
    .package = "snedata"
  )

  expect_true(file.exists(sentinel))
  expect_equal(list.files(tmpdir), basename(sentinel))
})
