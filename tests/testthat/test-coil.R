write_gray_png <- function(path, values = c(0, 0.25, 0.5, 1), size = 2) {
  png::writePNG(matrix(values, nrow = size), target = path)
}

write_rgb_png <- function(path, values = seq(0, 1, length.out = 12)) {
  png::writePNG(array(values, dim = c(2, 2, 3)), target = path)
}

file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE))
}

zip_is_available <- function() {
  nzchar(Sys.which("zip"))
}

with_temp_wd <- function(dir, code) {
  oldwd <- setwd(dir)
  on.exit(setwd(oldwd), add = TRUE)
  force(code)
}

make_png_zip <- function(zipfile, files = "obj1__0.png") {
  srcdir <- tempfile()
  dir.create(srcdir)
  write_gray_png(file.path(srcdir, "obj1__0.png"))
  with_temp_wd(srcdir, utils::zip(zipfile, files = files, flags = "-q"))
}

test_that("COIL filename parser reads object and pose ids", {
  parsed <- parse_coil_filename("obj5__150.png")

  expect_equal(parsed$object, 5L)
  expect_equal(parsed$pose, 150L)
  expect_error(parse_coil_filename("not-a-coil-file.png"), "must match")
  expect_error(parse_coil_filename("obj0__0.png", coil20_spec()), "Invalid")
  expect_error(parse_coil_filename("obj1__72.png", coil20_spec()), "Invalid")
  expect_error(parse_coil_filename("obj1__4.png", coil100_spec()), "Invalid")
})

test_that("pixel name helpers match documented order", {
  expect_equal(
    coil20_pixel_names(width = 2, height = 3),
    c("x1_y1", "x1_y2", "x1_y3", "x2_y1", "x2_y2", "x2_y3")
  )
  expect_equal(
    coil100_pixel_names(width = 2, height = 2),
    c(
      "r_x1_y1",
      "r_x1_y2",
      "r_x2_y1",
      "r_x2_y2",
      "g_x1_y1",
      "g_x1_y2",
      "g_x2_y1",
      "g_x2_y2",
      "b_x1_y1",
      "b_x1_y2",
      "b_x2_y1",
      "b_x2_y2"
    )
  )
})

test_that("read_coil_dir returns sorted data frames and matrix lists", {
  skip_if_not_installed("png")

  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gray_png(file.path(tmpdir, "obj2__1.png"), values = c(1, 0.5, 0.25, 0))
  write_gray_png(file.path(tmpdir, "obj1__0.png"), values = c(0, 0.25, 0.5, 1))

  df <- read_coil_dir(
    tmpdir,
    pixel_names = coil20_pixel_names(width = 2, height = 2)
  )
  mat <- read_coil_dir(
    tmpdir,
    as = "matrix",
    pixel_names = coil20_pixel_names(width = 2, height = 2)
  )

  expect_s3_class(df, "data.frame")
  expect_equal(rownames(df), c("1_0", "2_1"))
  expect_equal(names(df), c("x1_y1", "x1_y2", "x2_y1", "x2_y2", "Label"))
  expect_equal(as.character(df$Label), c("1", "2"))
  expect_named(mat, c("data", "labels", "poses", "ids"))
  expect_equal(dim(mat$data), c(2L, 4L))
  expect_equal(mat$ids, c("1_0", "2_1"))
  expect_equal(as.character(mat$labels), c("1", "2"))
})

test_that("RGB PNGs can be read into documented channel order", {
  skip_if_not_installed("png")

  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_rgb_png(file.path(tmpdir, "obj1__0.png"))

  df <- read_coil_dir(
    tmpdir,
    pixel_names = coil100_pixel_names(width = 2, height = 2)
  )

  expect_equal(names(df)[1:12], coil100_pixel_names(width = 2, height = 2))
  expect_equal(rownames(df), "1_0")
  expect_equal(as.character(df$Label), "1")
})

test_that("COIL readers report missing files, duplicate ids, and wrong shapes", {
  skip_if_not_installed("png")

  tmpdir <- tempfile()
  dir.create(tmpdir)
  expect_error(read_coil_dir(tmpdir), "No PNG files")

  nested <- file.path(tmpdir, "nested")
  dir.create(nested)
  write_gray_png(file.path(tmpdir, "obj1__0.png"), size = 128)
  write_gray_png(file.path(nested, "obj1__0.png"), size = 128)
  expect_error(
    read_coil_dir(tmpdir, recursive = TRUE, spec = coil20_spec()),
    "Duplicate COIL"
  )

  wrong_shape <- tempfile()
  dir.create(wrong_shape)
  write_gray_png(file.path(wrong_shape, "obj1__0.png"), size = 2)
  expect_error(
    read_coil_dir(wrong_shape, spec = coil20_spec()),
    "Expected COIL-20 image"
  )
})

test_that("complete COIL dataset validation is enforced at dataset layer", {
  metadata <- data.frame(
    file = "obj1__0.png",
    object = 1L,
    pose = 0L
  )
  expect_silent(validate_coil_metadata(metadata, spec = coil20_spec()))
  expect_error(
    validate_coil_metadata(metadata, spec = coil20_spec(), complete = TRUE),
    "exactly 1440 images"
  )
  expect_error(
    validate_coil_metadata(metadata, spec = coil100_spec(), complete = TRUE),
    "exactly 7200 images"
  )
})

test_that("zip entry validation rejects unsafe and duplicate normalized paths", {
  expect_silent(validate_zip_entries(c("coil/obj1__0.png")))
  expect_error(validate_zip_entries(c("../obj1__0.png")), "unsafe")
  expect_error(validate_zip_entries(c("/tmp/obj1__0.png")), "unsafe")
  expect_error(validate_zip_entries(c("C:/tmp/obj1__0.png")), "unsafe")
  expect_error(
    validate_zip_entries(c("coil/obj1__0.png", "coil\\\\obj1__0.png")),
    "duplicate"
  )
})

test_that("download_coil cleans an explicit destination file after reader errors", {
  skip_if_not_installed("png")
  skip_if_not(zip_is_available(), "zip command is not available")

  zipfile <- tempfile(fileext = ".zip")
  make_png_zip(zipfile)
  destfile <- tempfile()

  expect_error(
    download_coil(
      url = file_url(zipfile),
      file = destfile,
      cleanup = TRUE,
      spec = coil20_spec()
    ),
    "exactly 1440 images"
  )
  expect_false(file.exists(paste0(destfile, ".zip")))
  extract_dirs <- list.dirs(
    dirname(destfile),
    recursive = FALSE,
    full.names = TRUE
  )
  expect_false(any(grepl("^coil-extract-", basename(extract_dirs))))
})

test_that("read_coil_zip does not clean caller-supplied extraction directories", {
  exdir <- tempfile()
  dir.create(exdir)

  expect_error(read_coil_zip(tempfile(), exdir = exdir, cleanup = TRUE))
  expect_true(dir.exists(exdir))
})

test_that("show_coil_object plots data frames and matrix-list results", {
  images <- matrix(seq(0, 1, length.out = 128 * 128), nrow = 1)
  df <- format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil20_pixel_names(),
    label_levels = 1:20
  )
  mat <- format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil20_pixel_names(),
    label_levels = 1:20,
    as = "matrix"
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(show_coil_object(df, object = 1, pose = 0))
  expect_silent(show_coil_object(mat, object = 1, pose = 0))
  expect_error(show_coil_object(df, object = 1, pose = 1), "No row")
})

test_that("show_coil_object explains COIL-100 poses are angles", {
  images <- matrix(seq(0, 1, length.out = 128 * 128 * 3), nrow = 1)
  df <- format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil100_pixel_names(),
    label_levels = 1:100
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(show_coil_object(df, object = 1, pose = 0))
  expect_error(
    show_coil_object(df, object = 1, pose = 1),
    "COIL-100 uses viewing angles"
  )
})

test_that("PNG dependency guard is at the low-level reader path", {
  with_mocked_bindings(
    is_installed = function(pkgname) FALSE,
    expect_error(
      read_coil_png("unused.png"),
      "Please install the 'png' package"
    ),
    .package = "snedata"
  )
})
