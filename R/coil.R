# COIL image dataset support.

coil_base_url <- "https://cave.cs.columbia.edu/old/databases/SLAM_coil-20_coil-100"

#' Download COIL-20
#'
#' Download the processed Columbia Object Image Library COIL-20 dataset.
#'
#' Downloads and expands the processed COIL-20 ZIP archive and converts the PNG
#' files to a data frame or matrix-list result. COIL-20 contains 20 objects,
#' each represented by 72 grayscale poses. Each image has resolution 128 x 128.
#'
#' The optional [png](https://cran.r-project.org/package=png) package is
#' required when reading the downloaded PNG files.
#'
#' @format If `as = "data.frame"`, the default, a data frame with 16,385
#'   variables:
#'
#' * `x1_y1`, `x1_y2`, ... `x1_y128`, `x2_y1`, ... `x128_y128`: Pixel values
#'   ranging from 0 to 1. The `x` index increases from left to right, and the
#'   `y` index increases from top to bottom, so `x1_y1` is top left and
#'   `x128_y128` is bottom right.
#' * `Label`: The object id, stored as a factor with levels 1 to 20.
#'
#' Row names are `"<object>_<pose>"`, where `<object>` is the object id and
#' `<pose>` is the pose id, from 0 to 71.
#'
#' @param file File path to download the ZIP archive to. If `NULL`, a file in a
#'   temporary work directory is used.
#' @param cleanup If `TRUE`, delete temporary download and extraction files
#'   before returning. If `file` is supplied, only the downloaded ZIP and the
#'   dedicated extraction directory are removed.
#' @param verbose If `TRUE`, log download and extraction progress.
#' @param as Return format. Use `"data.frame"` for the original wide data frame
#'   shape, or `"matrix"` for a list with `data`, `labels`, `poses`, and `ids`.
#' @return If `as = "data.frame"`, a data frame containing the COIL-20 dataset.
#'   If `as = "matrix"`, a list containing a numeric matrix with one image per
#'   row, factor labels, integer poses, and row ids.
#' @references
#' The Columbia Object Image Library COIL-20
#' <https://cave.cs.columbia.edu/repository/COIL-20>
#' @export
#' @examples
#' \dontrun{
#' coil20 <- download_coil20(verbose = TRUE)
#' show_coil_object(coil20, object = 5, pose = 4)
#' }
download_coil20 <- function(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  download_coil(
    url = coil_url("coil-20", "coil-20-proc.zip"),
    file = file,
    cleanup = cleanup,
    verbose = verbose,
    as = as,
    spec = coil20_spec()
  )
}

#' Download COIL-100
#'
#' Download the Columbia Object Image Library COIL-100 dataset.
#'
#' Downloads and expands the COIL-100 ZIP archive and converts the PNG files to
#' a data frame or matrix-list result. COIL-100 contains 100 objects, each
#' represented by 72 RGB poses at five-degree viewing-angle increments. Each
#' image has resolution 128 x 128.
#'
#' The optional [png](https://cran.r-project.org/package=png) package is
#' required when reading the downloaded PNG files.
#'
#' @format If `as = "data.frame"`, the default, a data frame with 49,153
#'   variables:
#'
#' * `r_x1_y1`, `r_x1_y2`, ... `r_x128_y128`: Red-channel pixel values.
#' * `g_x1_y1`, `g_x1_y2`, ... `g_x128_y128`: Green-channel pixel values.
#' * `b_x1_y1`, `b_x1_y2`, ... `b_x128_y128`: Blue-channel pixel values.
#' * `Label`: The object id, stored as a factor with levels 1 to 100.
#'
#' Pixel values range from 0 to 1. Within each channel, the `x` index increases
#' from left to right and the `y` index increases from top to bottom.
#'
#' Row names are `"<object>_<angle>"`, where `<object>` is the object id and
#' `<angle>` is the viewing angle in degrees, from 0 to 355 in five-degree
#' increments.
#'
#' @inheritParams download_coil20
#' @return If `as = "data.frame"`, a data frame containing the COIL-100 dataset.
#'   If `as = "matrix"`, a list containing a numeric matrix with one image per
#'   row, factor labels, integer viewing angles, and row ids.
#' @references
#' The Columbia Object Image Library COIL-100
#' <https://cave.cs.columbia.edu/repository/COIL-100>
#' @export
#' @examples
#' \dontrun{
#' coil100 <- download_coil100(verbose = TRUE, as = "matrix")
#' show_coil_object(coil100, object = 5, pose = 150)
#' }
download_coil100 <- function(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  download_coil(
    url = coil_url("coil-100", "coil-100.zip"),
    file = file,
    cleanup = cleanup,
    verbose = verbose,
    as = as,
    spec = coil100_spec()
  )
}

#' Visualize a COIL object image
#'
#' Display a COIL-20 or COIL-100 object pose.
#'
#' @param df Data frame returned by [download_coil20()] or [download_coil100()].
#'   Matrix-list results returned with `as = "matrix"` are also supported.
#' @param object Object id to display. COIL-20 contains objects 1 to 20 and
#'   COIL-100 contains objects 1 to 100.
#' @param pose For COIL-20, the pose id to display, from 0 to 71. For COIL-100,
#'   the viewing angle, from 0 to 355 in five-degree increments.
#' @export
#' @examples
#' \dontrun{
#' coil20 <- download_coil20()
#' show_coil_object(coil20, object = 5, pose = 4)
#' }
show_coil_object <- function(df, object, pose) {
  features <- coil_feature_data(df)
  n_features <- ncol(features)
  name <- coil_row_name(object, pose)
  if (!(name %in% rownames(features))) {
    stop_missing_coil_object_pose(name, n_features)
  }

  img_data <- as.numeric(unlist(
    features[name, , drop = FALSE],
    use.names = FALSE
  ))
  if (n_features == coil20_n_pixels()) {
    dim(img_data) <- c(128, 128)
  } else if (n_features == coil100_n_pixels()) {
    dim(img_data) <- c(128, 128, 3)
  } else {
    stop(
      "`df` must contain either ",
      coil20_n_pixels(),
      " or ",
      coil100_n_pixels(),
      " pixel columns",
      call. = FALSE
    )
  }

  show_img(img_data)
}

coil_url <- function(...) {
  paste(coil_base_url, ..., sep = "/")
}

download_coil <- function(
  url,
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  spec
) {
  as <- match.arg(as)
  paths <- setup_coil_download_paths(url = url, file = file)
  if (cleanup) {
    on.exit(
      cleanup_owned_paths(paths$owned_paths, verbose = verbose),
      add = TRUE
    )
  }

  download_asset(url, paths$destfile, verbose = verbose)

  read_coil_zip(
    paths$destfile,
    exdir = paths$exdir,
    verbose = verbose,
    as = as,
    spec = spec,
    complete = TRUE
  )
}

setup_coil_download_paths <- function(url, file = NULL) {
  if (is.null(file)) {
    workdir <- tempfile("coil-")
    dir.create(workdir)
    return(list(
      destfile = file.path(workdir, basename(url)),
      exdir = file.path(workdir, "extracted"),
      owned_paths = workdir
    ))
  }

  destfile <- add_zip_extension(file)
  exdir <- tempfile("coil-extract-", tmpdir = dirname(destfile))
  list(
    destfile = destfile,
    exdir = exdir,
    owned_paths = c(destfile, exdir)
  )
}

read_coil_zip <- function(
  zipfile,
  exdir = NULL,
  cleanup = FALSE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  spec = NULL,
  complete = FALSE
) {
  as <- match.arg(as)
  owns_exdir <- is.null(exdir)
  if (owns_exdir) {
    exdir <- tempfile("coil-unzip-")
  }
  if (cleanup && owns_exdir) {
    on.exit(cleanup_owned_paths(exdir, verbose = verbose), add = TRUE)
  }

  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  entries <- zip_entries(zipfile)
  validate_zip_entries(entries)

  tsmessage(
    "Unzipping ",
    zipfile,
    " to ",
    exdir,
    " (this can take a long time)",
    force = verbose
  )
  utils::unzip(zipfile, files = entries, exdir = exdir)

  read_coil_dir(
    exdir,
    recursive = TRUE,
    verbose = verbose,
    as = as,
    spec = spec,
    complete = complete
  )
}

read_coil_dir <- function(
  dir,
  recursive = FALSE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL,
  spec = NULL,
  complete = FALSE
) {
  as <- match.arg(as)
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir, call. = FALSE)
  }

  files <- list.files(
    dir,
    full.names = TRUE,
    recursive = recursive,
    pattern = "\\.png$",
    ignore.case = TRUE
  )
  read_coil_files(
    files,
    verbose = verbose,
    as = as,
    pixel_names = pixel_names,
    spec = spec,
    complete = complete
  )
}

read_coil_files <- function(
  files,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL,
  spec = NULL,
  complete = FALSE
) {
  as <- match.arg(as)
  if (length(files) == 0) {
    stop("No PNG files found", call. = FALSE)
  }
  stop_if_not_installed("png")

  metadata <- coil_file_metadata(files, spec = spec)
  validate_coil_metadata(metadata, spec = spec, complete = complete)
  metadata <- metadata[order(metadata$object, metadata$pose), , drop = FALSE]
  files <- metadata$file

  first_img <- read_coil_png(files[1], spec = spec)
  n_pixels <- length(first_img)
  images <- matrix(0, nrow = length(files), ncol = n_pixels)
  images[1, ] <- as.vector(first_img)

  if (length(files) > 1) {
    for (i in seq_along(files)[-1]) {
      if (i %% 100 == 0) {
        tsmessage("Reading file ", i, " of ", length(files), force = verbose)
      }
      images[i, ] <- as.vector(read_coil_png(files[i], spec = spec))
    }
  }

  format_coil_result(
    images,
    objects = metadata$object,
    poses = metadata$pose,
    pixel_names = if (is.null(pixel_names)) {
      if (is.null(spec)) NULL else spec$pixel_names()
    } else {
      pixel_names
    },
    label_levels = if (is.null(spec)) NULL else spec$object_range,
    as = as
  )
}

read_coil_png <- function(file, spec = NULL) {
  stop_if_not_installed("png")
  img <- png::readPNG(file)
  if (!is.null(spec) && !identical(dim(img), spec$dim)) {
    stop(
      "Expected ",
      spec$name,
      " image ",
      basename(file),
      " to have dimensions ",
      paste(spec$dim, collapse = " x "),
      " but found ",
      paste(dim(img), collapse = " x "),
      call. = FALSE
    )
  }
  img
}

format_coil_result <- function(
  images,
  objects,
  poses,
  pixel_names = NULL,
  label_levels = NULL,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  if (nrow(images) != length(objects) || length(objects) != length(poses)) {
    stop("Image, object, and pose counts must match", call. = FALSE)
  }

  if (is.null(pixel_names)) {
    pixel_names <- paste0("px", seq_len(ncol(images)))
  }
  if (length(pixel_names) != ncol(images)) {
    stop("Pixel name count must match image column count", call. = FALSE)
  }
  if (is.null(label_levels)) {
    label_levels <- sort(unique(objects))
  }

  ids <- coil_row_name(objects, poses)
  labels <- factor(objects, levels = label_levels)

  colnames(images) <- pixel_names
  rownames(images) <- ids

  if (as == "matrix") {
    return(list(
      data = images,
      labels = labels,
      poses = poses,
      ids = ids
    ))
  }

  df <- as.data.frame(images)
  df$Label <- labels
  rownames(df) <- ids
  df
}

coil_file_metadata <- function(files, spec = NULL) {
  parsed <- lapply(files, parse_coil_filename, spec = spec)
  data.frame(
    file = files,
    object = vapply(parsed, `[[`, integer(1), "object"),
    pose = vapply(parsed, `[[`, integer(1), "pose"),
    stringsAsFactors = FALSE
  )
}

parse_coil_filename <- function(file, spec = NULL) {
  match <- regmatches(
    basename(file),
    regexec("^obj([0-9]+)__([0-9]+)\\.png$", basename(file), ignore.case = TRUE)
  )[[1]]

  if (length(match) != 3) {
    stop(
      "PNG file names must match 'obj<object>__<pose>.png': ",
      basename(file),
      call. = FALSE
    )
  }

  parsed <- list(
    object = as.integer(match[2]),
    pose = as.integer(match[3])
  )
  if (!is.null(spec)) {
    validate_coil_id(parsed$object, spec$object_range, "object", basename(file))
    validate_coil_id(parsed$pose, spec$pose_range, "pose", basename(file))
  }
  parsed
}

validate_coil_id <- function(id, valid, field, file) {
  if (is.na(id) || !(id %in% valid)) {
    stop(
      "Invalid COIL ",
      field,
      " id in ",
      file,
      ": ",
      id,
      ". Expected one of ",
      min(valid),
      " through ",
      max(valid),
      call. = FALSE
    )
  }
}

validate_coil_metadata <- function(metadata, spec = NULL, complete = FALSE) {
  ids <- coil_row_name(metadata$object, metadata$pose)
  duplicated_ids <- unique(ids[duplicated(ids)])
  if (length(duplicated_ids) > 0) {
    stop(
      "Duplicate COIL object/pose ids: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(spec) && complete) {
    expected <- expand.grid(
      object = spec$object_range,
      pose = spec$pose_range,
      KEEP.OUT.ATTRS = FALSE
    )
    expected_ids <- coil_row_name(expected$object, expected$pose)
    missing <- setdiff(expected_ids, ids)
    extra <- setdiff(ids, expected_ids)
    if (length(missing) > 0 || length(extra) > 0) {
      stop(
        spec$name,
        " dataset must contain exactly ",
        length(expected_ids),
        " images; found ",
        nrow(metadata),
        ". Missing ",
        length(missing),
        " and unexpected ",
        length(extra),
        " object/pose ids.",
        call. = FALSE
      )
    }
  }

  invisible(metadata)
}

zip_entries <- function(zipfile) {
  info <- utils::unzip(zipfile, list = TRUE)
  entries <- info$Name
  if (length(entries) == 0) {
    stop("Zip file contains no entries: ", zipfile, call. = FALSE)
  }
  entries
}

validate_zip_entries <- function(entries) {
  normalized <- normalize_zip_entries(entries)
  unsafe <- is.na(normalized)
  if (any(unsafe)) {
    stop(
      "Zip file contains unsafe paths: ",
      paste(entries[unsafe], collapse = ", "),
      call. = FALSE
    )
  }
  duplicates <- unique(normalized[duplicated(normalized)])
  if (length(duplicates) > 0) {
    stop(
      "Zip file contains duplicate target paths: ",
      paste(duplicates, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(entries)
}

normalize_zip_entries <- function(entries) {
  vapply(entries, normalize_zip_entry, character(1))
}

normalize_zip_entry <- function(entry) {
  entry <- gsub("\\\\", "/", entry)
  if (entry == "" || grepl("^/", entry) || grepl("^[A-Za-z]:", entry)) {
    return(NA_character_)
  }

  parts <- strsplit(entry, "/+", perl = TRUE)[[1]]
  if (any(parts %in% c("", ".", ".."))) {
    return(NA_character_)
  }
  paste(parts, collapse = "/")
}

coil20_spec <- function() {
  list(
    name = "COIL-20",
    dim = c(128L, 128L),
    object_range = 1:20,
    pose_range = 0:71,
    pixel_names = coil20_pixel_names
  )
}

coil100_spec <- function() {
  list(
    name = "COIL-100",
    dim = c(128L, 128L, 3L),
    object_range = 1:100,
    pose_range = seq(0, 355, by = 5),
    pixel_names = coil100_pixel_names
  )
}

coil20_pixel_names <- function(width = 128, height = 128) {
  paste0(
    "x",
    rep(seq_len(width), each = height),
    "_y",
    rep(seq_len(height), times = width)
  )
}

coil100_pixel_names <- function(width = 128, height = 128) {
  n_channel_pixels <- width * height
  paste0(
    rep(c("r", "g", "b"), each = n_channel_pixels),
    "_x",
    rep(rep(seq_len(width), each = height), times = 3),
    "_y",
    rep(seq_len(height), times = width * 3)
  )
}

coil20_n_pixels <- function() {
  128 * 128
}

coil100_n_pixels <- function() {
  128 * 128 * 3
}

coil_row_name <- function(object, pose) {
  paste(object, pose, sep = "_")
}

coil_feature_data <- function(df) {
  if (is.list(df) && !is.null(df$data)) {
    data <- df$data
  } else if (is.matrix(df)) {
    data <- df
  } else if (is.data.frame(df)) {
    data <- df[, -ncol(df), drop = FALSE]
  } else {
    stop(
      "`df` must be a data frame, matrix, or matrix-list result",
      call. = FALSE
    )
  }

  if (is.null(rownames(data))) {
    stop("`df` must have row names in '<object>_<pose>' form", call. = FALSE)
  }
  data
}

stop_missing_coil_object_pose <- function(name, n_features) {
  message <- paste0("No row with object_pose: ", name)
  if (n_features == coil100_n_pixels()) {
    message <- paste0(
      message,
      ". COIL-100 uses viewing angles for `pose`; valid values are ",
      "0, 5, 10, ..., 355."
    )
  } else if (n_features == coil20_n_pixels()) {
    message <- paste0(message, ". COIL-20 uses pose ids from 0 to 71.")
  }

  stop(message, call. = FALSE)
}

show_img <- function(img, x1 = 100, x2 = 250, y1 = 300, y2 = 450) {
  graphics::plot(
    c(x1, x2),
    c(y1, y2),
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  graphics::rasterImage(img, x1, y1, x2, y2, interpolate = FALSE)
}

add_zip_extension <- function(file) {
  if (grepl("\\.zip$", file, ignore.case = TRUE)) {
    return(file)
  }
  paste0(file, ".zip")
}
