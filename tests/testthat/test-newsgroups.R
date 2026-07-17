newsgroup_fixture_records <- list(
  list(
    "train",
    "alt.atheism",
    "10",
    c("Subject: train atheist 10", "", "Body one")
  ),
  list(
    "train",
    "alt.atheism",
    "2",
    c("Subject: train atheist 2", "", "Body two")
  ),
  list(
    "train",
    "comp.graphics",
    "30",
    c("Subject: train graphics", "", "Body three")
  ),
  list(
    "test",
    "alt.atheism",
    "11",
    c("Subject: test atheist", "", "Body four")
  ),
  list(
    "test",
    "comp.graphics",
    "12",
    c("Subject: test graphics 12", "", "Body five")
  ),
  list(
    "test",
    "comp.graphics",
    "4",
    c("Subject: test graphics 4", "", "Body six")
  )
)

write_newsgroup_fixture <- function(
  root,
  creation_order = seq_along(newsgroup_fixture_records)
) {
  for (i in creation_order) {
    record <- newsgroup_fixture_records[[i]]
    path <- file.path(
      root,
      paste0("20news-bydate-", record[[1]]),
      record[[2]]
    )
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(record[[4]], file.path(path, record[[3]]))
  }
}

canonical_newsgroup_levels <- c(
  "alt.atheism",
  "comp.graphics",
  "comp.os.ms-windows.misc",
  "comp.sys.ibm.pc.hardware",
  "comp.sys.mac.hardware",
  "comp.windows.x",
  "misc.forsale",
  "rec.autos",
  "rec.motorcycles",
  "rec.sport.baseball",
  "rec.sport.hockey",
  "sci.crypt",
  "sci.electronics",
  "sci.med",
  "sci.space",
  "soc.religion.christian",
  "talk.politics.guns",
  "talk.politics.mideast",
  "talk.politics.misc",
  "talk.religion.misc"
)

test_that("20 Newsgroups reader uses stable source identity", {
  root <- tempfile()
  reverse_root <- tempfile()
  on.exit(unlink(c(root, reverse_root), recursive = TRUE), add = TRUE)
  write_newsgroup_fixture(root)
  write_newsgroup_fixture(
    reverse_root,
    creation_order = rev(seq_along(newsgroup_fixture_records))
  )

  df <- snedata:::read_newsgroups_data(root, subset = "all")
  reverse_df <- snedata:::read_newsgroups_data(reverse_root, subset = "all")

  expect_identical(df, reverse_df)
  expect_s3_class(df, "data.frame")
  expect_identical(
    names(df),
    c("Id", "FileId", "Text", "Subset", "Label", "Newsgroup")
  )
  expect_identical(
    df$Id,
    c(
      "train/alt.atheism/10",
      "train/alt.atheism/2",
      "train/comp.graphics/30",
      "test/alt.atheism/11",
      "test/comp.graphics/12",
      "test/comp.graphics/4"
    )
  )
  expect_identical(df$FileId, c("10", "2", "30", "11", "12", "4"))
  expect_type(df$FileId, "character")
  expect_identical(levels(df$Subset), c("train", "test"))
  expect_identical(levels(df$Newsgroup), canonical_newsgroup_levels)
  expect_identical(levels(df$Label), as.character(0:19))
  expect_identical(
    as.character(df$Label),
    c("0", "0", "1", "0", "1", "1")
  )
  expect_identical(
    df$Text[[1]],
    "Subject: train atheist 10\n\nBody one"
  )
})

test_that("20 Newsgroups identity is unchanged by subset selection", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  write_newsgroup_fixture(root)

  all <- snedata:::read_newsgroups_data(root, subset = "all")
  train <- snedata:::read_newsgroups_data(root, subset = "train")
  test <- snedata:::read_newsgroups_data(root, subset = "test")

  expect_identical(train, all[all$Subset == "train", ])
  expect_identical(test, all[all$Subset == "test", ])
  expect_identical(levels(train$Newsgroup), canonical_newsgroup_levels)
  expect_identical(levels(test$Newsgroup), canonical_newsgroup_levels)
  expect_identical(levels(train$Label), as.character(0:19))
  expect_identical(levels(test$Label), as.character(0:19))
})

test_that("20 Newsgroups reader rejects unknown group directories", {
  root <- tempfile()
  group_dir <- file.path(root, "not.a.newsgroup")
  dir.create(group_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("Message text", file.path(group_dir, "1"))

  expect_error(
    snedata:::read_newsgroup_directory(group_dir),
    "Unknown 20 Newsgroups directory: not.a.newsgroup",
    fixed = TRUE
  )
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
