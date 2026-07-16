#' Download Small NORB
#'
#' Download Small NORB database of images of toys.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame or a matrix/list result.
#'
#' The Small NORB dataset contains images of 50 toys. The toys are divided into
#' five categories (animal, human, airplane, truck, car) with ten examples per
#' category. Each object was then images under 6 different lighting conditions,
#' 9 elevations and 18 different azimuths, so there are 972 images per toy. The
#' process was then repeated with a different camera, so there are actually
#' 1,944 images per toy. This dataset stores each pair of images for a
#' given toy, lighting, elevation and azimuth as a single row. Each image is
#' 96 by 96 pixels, so the first 9,216 columns contain the pixels of the first
#' image, and the second 9,216 (`9217:18432`) columns contain the pixels of
#' the second image. The other information (lighting and so on) are also stored
#' as factors.
#'
#' @format A data frame with 18,439 variables:
#'
#' * `c0px1`, `c0px2`, `c0px3` ... `c0px9216`: Integer
#'   pixel value, from 0 (white) to 255 (black) for the first image in the
#'   pair
#' * `c1px1`, `c1px2`, `c1px3` ... `c1px9216`: Integer
#'   pixel value, from 0 (white) to 255 (black) for the second image in the
#'   pair
#' * `Instance`: The index of the toy in a particular category,
#'   represented by a factor in the range 0-9. The training set consists of
#'   instances 0, 1, 2, 3 and 5, and the test set consists of 4, 6, 7, 8 and 9.
#' * `Elevation`: The elevation of the camera represented by a
#'   factor in the range 0-8. These represent elevations of 30 to 70 degrees
#'   from the horizontal, in increments of 5 degrees.
#' * `Azimuth`: The azimuth, represented by a factor in the range
#'   0, 2, 4 .. 34. Multiply by ten to get the value in degrees.
#' * `Lighting`: The lighting condition, represened by a factor in
#'   the range 0-5.
#' * `Label`: The toy category, represented by a factor in the
#'   range 0-4.
#' * `Split`: Whether the toy in is in the `training` or
#'   `testing` set, represented by a factor
#' * `Description`: The name of the toy category associated with
#'   `Label`, represented by a factor.
#'
#' The pixel features are organized row-wise from the top left of each image.
#' The `Label` levels correspond to:
#' * `0`: Four-legged animal
#' * `1`: Human figure
#' * `2`: Airplane
#' * `3`: Truck
#' * `4`: Car
#'
#' There are 48,600 items in the data set. The first 24,300 are the training
#' set, and the remaining 24,300 are the testing set, but you can also use
#' the `Split` column to determine which split a given row is in.
#'
#' Items in the dataset can be visualized with the
#' [show_norb_object()] function.
#'
#' For more information see
#' <https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/>.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param split Which split to download. Use `"all"` for both the training
#'   and testing sets, or `"training"` or `"testing"` for one split.
#' @param as Return format. Use `"data.frame"` for the original data frame
#'   shape, or `"matrix"` for a list with `data` and `meta`. The pixel matrix
#'   alone requires about 3.34 GiB; use `"matrix"` to avoid the wide data-frame
#'   conversion.
#' @return If `as = "data.frame"`, a data frame containing the Small NORB
#'   dataset. If `as = "matrix"`, a list with `data`, an integer
#'   matrix with one image pair per row, and `meta`, a data frame with the
#'   non-pixel metadata columns.
#' @export
#' @examples
#' \dontrun{
#' # download the data set without making a wide data frame
#' norb <- download_norb_small(verbose = TRUE, as = "matrix")
#'
#' # first 24,300 instances are the training set
#' norb_train <- head(norb$data, 24300)
#' # the remaining 24,300 are the test set
#' norb_test <- tail(norb$data, 24300)
#'
#' # Or equivalently
#' norb_train2 <- norb$data[norb$meta$Split == "training", ]
#' norb_test2 <- norb$data[norb$meta$Split == "testing", ]
#'
#' identical(norb_train, norb_train2) # TRUE
#' identical(norb_test, norb_test2) # also TRUE
#'
#' # PCA on 1000 examples
#' sample_rows <- sample(nrow(norb$data), 1000)
#' norb_r1000 <- norb$data[sample_rows, ]
#' pca <- prcomp(norb_r1000, retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = norb$meta$Label[sample_rows],
#'   col = rainbow(length(levels(norb$meta$Label)))[norb$meta$Label[sample_rows]]
#' )
#'
#' # The legacy wide data-frame result remains available explicitly:
#' norb_df <- download_norb_small(verbose = TRUE, as = "data.frame")
#' }
#' @references
#' The Small NORB Dataset, v1.0
#' <https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/>
#'
#' LeCun, Y., Huang, F. J., & Bottou, L. (2004, June).
#' Learning methods for generic object recognition with invariance to pose and lighting.
#' In *IEEE Computer Society Conference on Computer Vision and Pattern Recognition (CVPR) 2004*
#' (pp. 97-104).
#' IEEE.
#' <http://doi.ieeecomputersociety.org/10.1109/CVPR.2004.144>
#' @export
download_norb_small <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  verbose = FALSE,
  split = c("all", "training", "testing"),
  as = c("data.frame", "matrix")
) {
  split <- match.arg(split)
  as <- match.arg(as)
  warn_wide_data_frame(
    "Small NORB",
    n_rows = norb_split_size(split),
    n_cols = 96L * 96L * 2L,
    storage = "integer",
    as = as
  )
  if (split != "all") {
    return(read_norb_data(
      base_url = base_url,
      split = split,
      verbose = verbose,
      as = as
    ))
  }

  read_norb_all_data(base_url = base_url, verbose = verbose, as = as)
}

#' Visualize NORB object.
#'
#' Display a NORB object as an image pair.
#'
#' The NORB objects as stored as image pairs and displayed with the first image
#' in the pair on top, and the second image in the pair below it.
#'
#' @param df Data frame containing NORB objects.
#' @param category The category of the object, an integer from 0 to 4.
#' @param instance The instance in the category, an integer from 0 to 9.
#' @param elevation The elevation of the camera, an integer from 0 to 8,
#'   representing elevations of 30 to 70 degrees from the horizontal, in
#'   increments of 5 degrees.
#' @param azimuth The azimuth, an even integer from 0, 2, 4, ... to 34.
#'   Multiply by ten to get the value in degrees.
#' @param lighting The lighting condition, represened by an integer in
#'   the range 0-5.
#' @examples
#' \dontrun{
#' show_norb_object(norb,
#'   category = 2, instance = 6, elevation = 6,
#'   azimuth = 24, lighting = 2
#' )
#' # Compare with example at https://github.com/ndrplz/small_norb
#' }
#' @export
show_norb_object <- function(
  df,
  category = 0,
  instance = 0,
  elevation = 0,
  azimuth = 0,
  lighting = 0
) {
  x <- df[
    df$Label == category &
      df$Instance == instance &
      df$Elevation == elevation &
      df$Azimuth == azimuth &
      df$Lighting == lighting,
  ]
  if (nrow(x) != 1) {
    stop("Found ", nrow(x), " entries, but need 1")
  }
  show_norb_vec(as.numeric(x[, 1:(96 * 96 * 2)]))
  title <- paste0(
    c(
      "Category:",
      category,
      "Instance:",
      instance
    ),
    collapse = " "
  )
  sub <- paste0(
    c(
      "Elevation:",
      elevation,
      "Azimuth:",
      azimuth,
      "Lighting:",
      lighting
    ),
    collapse = " "
  )
  graphics::title(main = title, sub = sub)
}

read_norb_data <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  split = "training",
  verbose = TRUE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  ids <- switch(
    split,
    training = "46789",
    testing = "01235",
    stop("Unknown split '", split, "'")
  )

  file_base <- paste0("smallnorb-5x", ids, "x9x18x6x2x96x96-", split)

  images <- read_norb_images(
    file = paste0(file_base, "-dat.mat.gz"),
    base_url = base_url,
    verbose = verbose
  )
  cats <- read_norb_categories(
    file = paste0(file_base, "-cat.mat.gz"),
    base_url = base_url,
    verbose = verbose
  )
  info <- read_norb_info(
    file = paste0(file_base, "-info.mat.gz"),
    base_url = base_url,
    verbose = verbose
  )

  validate_norb_asset_counts(images, info, cats, split)
  format_norb_result(images, info, cats, split = split, as = as)
}

validate_norb_asset_counts <- function(images, info, cats, split) {
  image_count <- nrow(images)
  info_count <- ncol(info)
  category_count <- length(cats)

  if (image_count != info_count || image_count != category_count) {
    stop(
      "NORB ",
      split,
      " asset count mismatch: image count ",
      image_count,
      ", info count ",
      info_count,
      ", category count ",
      category_count,
      "; expected matching counts",
      call. = FALSE
    )
  }
}

norb_pixel_names <- function(n_pixels = 96 * 96 * 2) {
  if (n_pixels %% 2 != 0) {
    stop("NORB pixel matrix must have a row count divisible by 2")
  }
  n_camera_pixels <- n_pixels / 2
  c(
    paste0("c0px", seq_len(n_camera_pixels)),
    paste0("c1px", seq_len(n_camera_pixels))
  )
}

norb_description_levels <- function() {
  c("Animal", "Human", "Airplane", "Truck", "Car")
}

format_norb_result <- function(
  images,
  info,
  cats,
  split,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  data <- images
  colnames(data) <- norb_pixel_names(ncol(images))
  meta <- format_norb_meta(info, cats, split = split, n_images = nrow(images))

  if (as == "matrix") {
    return(list(data = data, meta = meta))
  }

  data.frame(data, meta)
}

format_norb_meta <- function(info, cats, split, n_images) {
  if (ncol(info) != n_images) {
    stop(
      "Info column count (",
      ncol(info),
      ") does not match image count (",
      n_images,
      ")"
    )
  }
  if (length(cats) != n_images) {
    stop(
      "Category count (",
      length(cats),
      ") does not match image count (",
      n_images,
      ")"
    )
  }

  rownames(info) <- c("Instance", "Elevation", "Azimuth", "Lighting")
  split_col <- factor(rep(split, n_images), levels = c("training", "testing"))
  meta <- data.frame(t(info), Split = split_col, Label = cats)
  meta$Instance <- factor(meta$Instance, levels = 0:9)
  meta$Elevation <- factor(meta$Elevation, levels = 0:8)
  meta$Azimuth <- factor(meta$Azimuth, levels = seq(0, 34, 2))
  meta$Lighting <- factor(meta$Lighting, levels = 0:5)
  meta$Label <- factor(meta$Label, levels = 0:4)
  meta$Description <- meta$Label
  levels(meta$Description) <- norb_description_levels()

  meta
}

read_norb_all_data <- function(base_url, verbose, as) {
  training <- read_norb_data(
    base_url = base_url,
    split = "training",
    verbose = verbose,
    as = "matrix"
  )
  n_training <- nrow(training$data)
  n_features <- ncol(training$data)
  data <- matrix(0L, nrow = 2L * n_training, ncol = n_features)
  data[seq_len(n_training), ] <- training$data
  colnames(data) <- colnames(training$data)
  meta <- training$meta
  training <- NULL
  gc(FALSE)

  testing <- read_norb_data(
    base_url = base_url,
    split = "testing",
    verbose = verbose,
    as = "matrix"
  )
  n_testing <- nrow(testing$data)
  if (n_testing != n_training || ncol(testing$data) != n_features) {
    stop("NORB training and testing image dimensions must match", call. = FALSE)
  }
  data[n_training + seq_len(n_testing), ] <- testing$data
  meta <- rbind(meta, testing$meta)
  testing <- NULL
  gc(FALSE)

  if (as == "matrix") {
    return(list(data = data, meta = meta))
  }
  data.frame(data, meta)
}

norb_split_size <- function(split) {
  switch(
    split,
    training = 24300L,
    testing = 24300L,
    all = 48600L,
    stop("Unknown split '", split, "'", call. = FALSE)
  )
}

read_norb_images <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  file = "smallnorb-5x46789x9x18x6x2x96x96-training-dat.mat.gz",
  verbose = TRUE
) {
  f <- open_binary_file(base_url = base_url, filename = file, verbose)
  on.exit(close(f), add = TRUE)
  asset <- paste0("NORB image asset '", file, "'")
  read_norb_header(f, type = "byte", expected_ndim = 4L, asset = asset)
  dimensions <- read_norb_dimensions(
    f,
    n = 4L,
    asset = asset,
    dimension_names = c("n_images", "n_cameras", "n_rows", "n_cols")
  )
  validate_norb_image_dimensions(dimensions, asset)
  value_count <- binary_safe_product(dimensions, asset)
  values <- read_binary_exact(
    f,
    what = "integer",
    n = value_count,
    size = 1L,
    signed = FALSE,
    endian = "little",
    asset = asset,
    component = "image payload",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  matrix(
    values,
    nrow = dimensions[["n_images"]],
    ncol = dimensions[["n_cameras"]] *
      dimensions[["n_rows"]] *
      dimensions[["n_cols"]],
    byrow = TRUE
  )
}

read_norb_categories <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  file = "smallnorb-5x46789x9x18x6x2x96x96-training-cat.mat.gz",
  verbose = TRUE
) {
  f <- open_binary_file(base_url = base_url, filename = file, verbose)
  on.exit(close(f), add = TRUE)
  asset <- paste0("NORB category asset '", file, "'")
  read_norb_header(f, type = "integer", expected_ndim = 1L, asset = asset)
  dimensions <- read_norb_dimensions(
    f,
    n = 3L,
    asset = asset,
    dimension_names = c("n_images", "padding_2", "padding_3")
  )
  validate_norb_padding(dimensions[2:3], asset, header = dimensions)
  count <- binary_safe_product(dimensions[["n_images"]], asset, dimensions)
  categories <- read_binary_exact(
    f,
    what = "integer",
    n = count,
    size = 4L,
    signed = TRUE,
    endian = "little",
    asset = asset,
    component = "category payload",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  categories
}

read_norb_info <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  file = "smallnorb-5x46789x9x18x6x2x96x96-training-info.mat.gz",
  verbose = TRUE
) {
  f <- open_binary_file(base_url = base_url, filename = file, verbose)
  on.exit(close(f), add = TRUE)
  asset <- paste0("NORB metadata asset '", file, "'")
  read_norb_header(f, type = "integer", expected_ndim = 2L, asset = asset)
  dimensions <- read_norb_dimensions(
    f,
    n = 3L,
    asset = asset,
    dimension_names = c("n_images", "n_features", "padding_3")
  )
  validate_norb_padding(dimensions[["padding_3"]], asset, header = dimensions)

  if (dimensions[["n_features"]] != 4L) {
    stop(
      asset,
      " has an invalid metadata feature count: expected count 4 features ",
      "(16 bytes per image); actual count ",
      dimensions[["n_features"]],
      " features; header dimensions: ",
      binary_header_context(dimensions),
      call. = FALSE
    )
  }

  value_count <- binary_safe_product(
    dimensions[c("n_images", "n_features")],
    asset,
    header = dimensions
  )
  values <- read_binary_exact(
    f,
    what = "integer",
    n = value_count,
    size = 4L,
    signed = TRUE,
    endian = "little",
    asset = asset,
    component = "metadata payload",
    header = dimensions
  )
  assert_binary_eof(f, asset, header = dimensions)

  matrix(
    values,
    nrow = dimensions[["n_features"]],
    ncol = dimensions[["n_images"]]
  )
}

show_norb_vec <- function(x) {
  nc <- length(x) / 96
  graphics::image(
    matrix(x, nrow = 96, ncol = nc)[, nc:1],
    col = grDevices::gray(1:255 / 255)
  )
}

# - 0x1E3D4C51 for a single precision matrix
# - 0x1E3D4C52 for a packed matrix
# - 0x1E3D4C53 for a double precision matrix
# - 0x1E3D4C54 for an integer matrix
# - 0x1E3D4C55 for a byte matrix
# - 0x1E3D4C56 for a short matrix
# Function uses the equivalent integer string
magic_to_matrix_type <- function(magic) {
  switch(
    magic,
    "81766130" = "single precision",
    "82766130" = "packed",
    "83766130" = "double precision",
    "84766130" = "integer",
    "85766130" = "byte",
    "86766130" = "short",
    "Unknown"
  )
}

# Convert a binary matrix type to its magic number indentifier
matrix_type_to_magic <- function(type) {
  switch(
    type,
    "single precision" = "81766130",
    packed = "82766130",
    "double precision" = "83766130",
    integer = "84766130",
    byte = "85766130",
    short = "86766130",
    stop("Bad binary matrix type '", type, "'")
  )
}

read_norb_header <- function(f, type, expected_ndim, asset) {
  magic_bytes <- read_binary_exact(
    f,
    what = "integer",
    n = 4L,
    size = 1L,
    signed = FALSE,
    endian = "little",
    asset = asset,
    component = "matrix magic"
  )
  actual_magic <- paste0(magic_bytes, collapse = "")
  expected_magic <- matrix_type_to_magic(type)

  if (actual_magic != expected_magic) {
    stop(
      asset,
      " has an invalid matrix magic: expected ",
      type,
      " matrix (4 bytes); actual ",
      magic_to_matrix_type(actual_magic),
      " (",
      actual_magic,
      "); header dimensions unavailable",
      call. = FALSE
    )
  }

  dimension_header <- read_binary_exact(
    f,
    what = "integer",
    n = 4L,
    size = 1L,
    signed = FALSE,
    endian = "little",
    asset = asset,
    component = "dimension header",
    header = c(matrix_magic = actual_magic)
  )
  names(dimension_header) <- c(
    "n_dimensions",
    "padding_1",
    "padding_2",
    "padding_3"
  )

  if (dimension_header[["n_dimensions"]] != expected_ndim) {
    stop(
      asset,
      " has an invalid dimension count: expected count ",
      expected_ndim,
      " dimensions (1 byte); actual count ",
      dimension_header[["n_dimensions"]],
      " dimensions (1 byte); header dimensions: ",
      binary_header_context(dimension_header),
      call. = FALSE
    )
  }

  validate_norb_padding(
    dimension_header[c("padding_1", "padding_2", "padding_3")],
    asset,
    header = dimension_header,
    size = 1L
  )
}

read_norb_dimensions <- function(f, n, asset, dimension_names) {
  dimensions <- read_binary_exact(
    f,
    what = "integer",
    n = n,
    size = 4L,
    signed = TRUE,
    endian = "little",
    asset = asset,
    component = "dimension values"
  )
  names(dimensions) <- dimension_names
  dimensions
}

validate_norb_padding <- function(padding, asset, header, size = 4L) {
  actual_nonzero <- sum(padding != 0L)

  if (actual_nonzero != 0L) {
    stop(
      asset,
      " has invalid padding: expected count ",
      length(padding),
      " zero values (",
      length(padding) * size,
      " bytes); actual count ",
      actual_nonzero,
      " nonzero values; header dimensions: ",
      binary_header_context(header),
      call. = FALSE
    )
  }
}

validate_norb_image_dimensions <- function(dimensions, asset) {
  pixels_per_image <- binary_safe_product(
    dimensions[c("n_cameras", "n_rows", "n_cols")],
    asset,
    header = dimensions
  )
  expected_pixels_per_image <- 2L * 96L * 96L

  if (pixels_per_image != expected_pixels_per_image) {
    stop(
      asset,
      " has invalid image dimensions: expected count ",
      expected_pixels_per_image,
      " pixels (",
      expected_pixels_per_image,
      " bytes) per image; actual count ",
      pixels_per_image,
      " pixels per image; header dimensions: ",
      binary_header_context(dimensions),
      call. = FALSE
    )
  }

  binary_safe_product(dimensions, asset)
}

# Converts the NORB Small website magic hex numbers to integers
# e.g. magic("1E3D4C51") # [1] 81 76 61 30
magic <- function(hexstr) {
  as.integer(as.hexmode(rev(substring(
    hexstr,
    seq(1, nchar(hexstr) - 1, 2),
    seq(2, nchar(hexstr), 2)
  ))))
}
