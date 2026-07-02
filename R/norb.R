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
#' image, and the second 9,216 (\code{9217:18432}) columns contain the pixels of
#' the second image. The other information (lighting and so on) are also stored
#' as factors.
#'
#' @format A data frame with 18,439 variables:
#'
#' \describe{
#' \item{\code{c0px1}, \code{c0px2}, \code{c0px3} ... \code{c0px9216}}{Integer
#'   pixel value, from 0 (white) to 255 (black) for the first image in the
#'   pair}
#' \item{\code{c1px1}, \code{c1px2}, \code{c1px3} ... \code{c1px9216}}{Integer
#'   pixel value, from 0 (white) to 255 (black) for the second image in the
#'   pair}
#' \item{\code{Instance}}{The index of the toy in a particular category,
#'   represented by a factor in the range 0-9. The training set consists of
#'   instances 0, 1, 2, 3 and 5, and the test set consists of 4, 6, 7, 8 and 9.}
#' \item{\code{Elevation}}{The elevation of the camera represented by a
#'   factor in the range 0-8. These represent elevations of 30 to 70 degrees
#'   from the horizontal, in increments of 5 degrees.}
#' \item{\code{Azimuth}}{The azimuth, represented by a factor in the range
#'   0, 2, 4 .. 34. Multiply by ten to get the value in degrees.}
#' \item{\code{Lighting}}{The lighting condition, represened by a factor in
#'   the range 0-5.}
#' \item{\code{Label}}{The toy category, represented by a factor in the
#'   range 0-4.}
#' \item{\code{Split}}{Whether the toy in is in the \code{training} or
#'   \code{testing} set, represented by a factor}
#' \item{\code{Description}}{The name of the toy category associated with
#'   \code{Label}, represented by a factor.}
#' }
#'
#' The pixel features are organized row-wise from the top left of each image.
#' The \code{Label} levels correspond to:
#' \describe{
#'   \item{\code{0}}{Four-legged animal}
#'   \item{\code{1}}{Human figure}
#'   \item{\code{2}}{Airplane}
#'   \item{\code{3}}{Truck}
#'   \item{\code{4}}{Car}
#' }
#'
#' There are 48,600 items in the data set. The first 24,300 are the training
#' set, and the remaining 24,300 are the testing set, but you can also use
#' the \code{Split} column to determine which split a given row is in.
#'
#' Items in the dataset can be visualized with the
#' \code{\link{show_norb_object}} function.
#'
#' For more information see
#' \url{https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/}.
#'
#' @param base_url Base URL that the files are located at.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @param split Which split to download. Use \code{"all"} for both the training
#'   and testing sets, or \code{"training"} or \code{"testing"} for one split.
#' @param as Return format. Use \code{"data.frame"} for the original data frame
#'   shape, or \code{"matrix"} for a list with \code{data} and \code{meta}.
#' @return If \code{as = "data.frame"}, a data frame containing the Small NORB
#'   dataset. If \code{as = "matrix"}, a list with \code{data}, an integer
#'   matrix with one image pair per row, and \code{meta}, a data frame with the
#'   non-pixel metadata columns.
#' @export
#' @examples
#' \dontrun{
#' # download the data set
#' norb <- download_norb_small(verbose = TRUE)
#'
#' # first 24,300 instances are the training set
#' norb_train <- head(norb, 24300)
#' # the remaining 24,300 are the test set
#' norb_test <- tail(norb, 24300)
#'
#' # Or equivalently
#' norb_train2 <- norb[norb$Split == "training", ]
#' norb_test2 <- norb[norb$Split == "testing", ]
#'
#' identical(norb_train, norb_train2) # TRUE
#' identical(norb_test, norb_test2) # also TRUE
#'
#' # PCA on 1000 examples
#' norb_r1000 <- norb[sample(nrow(norb), 1000), ]
#' pca <- prcomp(norb_r1000[, 1:(96 * 96 * 2)], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = "n")
#' text(pca$x[, 1:2],
#'   labels = norb_r1000$Label,
#'   col = rainbow(length(levels(norb$Label)))[norb_r1000$Label]
#' )
#' }
#' @references
#' The Small NORB Dataset, v1.0
#' \url{https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/}
#'
#' LeCun, Y., Huang, F. J., & Bottou, L. (2004, June).
#' Learning methods for generic object recognition with invariance to pose and lighting.
#' In \emph{IEEE Computer Society Conference on Computer Vision and Pattern Recognition (CVPR) 2004}
#' (pp. 97-104).
#' IEEE.
#' \url{http://doi.ieeecomputersociety.org/10.1109/CVPR.2004.144}
#' @export
download_norb_small <- function(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  verbose = FALSE,
  split = c("all", "training", "testing"),
  as = c("data.frame", "matrix")
) {
  split <- match.arg(split)
  as <- match.arg(as)
  if (split != "all") {
    return(read_norb_data(
      base_url = base_url,
      split = split,
      verbose = verbose,
      as = as
    ))
  }

  training <- read_norb_data(
    base_url = base_url,
    split = "training",
    verbose = verbose,
    as = as
  )
  testing <- read_norb_data(
    base_url = base_url,
    split = "testing",
    verbose = verbose,
    as = as
  )
  combine_norb_results(training, testing, as = as)
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

  format_norb_result(images, info, cats, split = split, as = as)
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
  data <- t(images)
  colnames(data) <- norb_pixel_names(nrow(images))
  meta <- format_norb_meta(info, cats, split = split, n_images = ncol(images))

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

combine_norb_results <- function(
  training,
  testing,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  if (as == "matrix") {
    return(list(
      data = rbind(training$data, testing$data),
      meta = rbind(training$meta, testing$meta)
    ))
  }

  rbind(training, testing)
}

read_norb_images <-
  function(
    base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
    file = "smallnorb-5x46789x9x18x6x2x96x96-training-dat.mat.gz",
    verbose = TRUE
  ) {
    f <- open_binary_file(base_url = base_url, filename = file, verbose)
    on.exit(close(f), add = TRUE)
    check_header(f, "byte")

    ndim <- readBin(
      f,
      what = "integer",
      n = 4,
      size = 1,
      signed = FALSE,
      endian = "little"
    )[1]
    if (ndim != 4) {
      stop("Was expecting 4 dimensions, but found ", ndim)
    }

    dims <- rep(0, ndim)
    for (i in 1:ndim) {
      dims[i] <- readBin(
        f,
        what = "integer",
        n = 1,
        size = 4,
        signed = TRUE,
        endian = "little"
      )
    }

    n_imgs <- dims[1]
    img_size <- dims[2] * dims[3] * dims[4]
    res <- matrix(nrow = img_size, ncol = n_imgs)

    for (i in 1:dims[1]) {
      res[, i] <- readBin(
        f,
        what = "integer",
        n = img_size,
        size = 1,
        signed = FALSE,
        endian = "little"
      )
    }

    res
  }

read_norb_categories <-
  function(
    base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
    file = "smallnorb-5x46789x9x18x6x2x96x96-training-cat.mat.gz",
    verbose = TRUE
  ) {
    f <- open_binary_file(base_url = base_url, filename = file, verbose)
    on.exit(close(f), add = TRUE)
    check_header(f, "integer")

    ndim <- readBin(
      f,
      what = "integer",
      n = 4,
      size = 1,
      signed = FALSE,
      endian = "little"
    )[1]
    if (ndim != 1) {
      stop("Was expecting 1 dimension, but found ", ndim)
    }

    n_imgs <- readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )

    # Ignore next two lines representing (non-existent in this case) 2nd
    # and 3rd dimension sizes
    readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )
    readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )

    res <- readBin(
      f,
      what = "integer",
      n = n_imgs,
      size = 4,
      signed = TRUE,
      endian = "little"
    )

    res
  }

read_norb_info <-
  function(
    base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
    file = "smallnorb-5x46789x9x18x6x2x96x96-training-info.mat.gz",
    verbose = TRUE
  ) {
    f <- open_binary_file(base_url = base_url, filename = file, verbose)
    on.exit(close(f), add = TRUE)
    check_header(f, "integer")

    ndim <- readBin(
      f,
      what = "integer",
      n = 4,
      size = 1,
      signed = FALSE,
      endian = "little"
    )[1]
    if (ndim != 2) {
      stop("Was expecting 1 dimension, but found ", ndim)
    }

    n_imgs <- readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )
    n_features <- readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )

    # Ignore next lines representing (non-existent in this case) 3rd dimension
    readBin(
      f,
      what = "integer",
      n = 1,
      size = 4,
      signed = TRUE,
      endian = "little"
    )

    res <- matrix(nrow = n_features, ncol = n_imgs)
    for (i in 1:n_imgs) {
      res[, i] <- readBin(
        f,
        what = "integer",
        n = n_features,
        size = 4,
        signed = TRUE,
        endian = "little"
      )
    }

    res
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

# Ensure we are reading the binary matrix file we think we should be
check_header <- function(f, type) {
  magic <- paste0(
    readBin(
      f,
      what = "integer",
      n = 4,
      size = 1,
      signed = FALSE,
      endian = "little"
    ),
    collapse = ""
  )
  if (magic != matrix_type_to_magic(type)) {
    stop(
      "Was expecting a ",
      type,
      " matrix, but found: ",
      magic_to_matrix_type(magic)
    )
  }
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
