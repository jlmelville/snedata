isomap_swiss_roll_url <- paste0(
  "https://web.archive.org/web/20151031205619id_/",
  "http://web.mit.edu/cocosci/isomap/swiss_roll_data.mat"
)

isomap_faces_url <- paste0(
  "https://web.archive.org/web/20150922051706id_/",
  "http://isomap.stanford.edu/face_data.mat.Z"
)

#' Download Isomap Swiss Roll
#'
#' Download the original Swiss-roll dataset used in Isomap examples.
#'
#' The live MIT URL for this dataset is no longer available, so the default URL
#' uses an archived copy from the Internet Archive.
#'
#' @format A data frame with 20,000 rows and 6 variables:
#'
#' * `x`, `y`, `z`: Coordinates of the points in the rolled
#'   three-dimensional space.
#' * `u`, `v`: Coordinates of the points in the unrolled
#'   two-dimensional space.
#' * `color`: A string representing a color in hex format, generated
#'   from `u`.
#'
#' @param url URL of the Matlab data file.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return Data frame containing the Isomap Swiss-roll dataset.
#' @note Requires the
#' [R.matlab](https://cran.r-project.org/package=R.matlab)
#' package to be installed.
#' @export
#' @examples
#' \dontrun{
#' swiss <- download_isomap_swiss_roll()
#' plot(swiss$x, swiss$y, col = swiss$color, pch = 20, cex = 0.1)
#' }
#' @references
#' Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000).
#' A global geometric framework for nonlinear dimensionality reduction.
#' *Science*, *290*(5500), 2319-2323.
download_isomap_swiss_roll <- function(
  url = isomap_swiss_roll_url,
  verbose = FALSE,
  timeout = 1800
) {
  mat <- read_isomap_mat_url(
    url = url,
    fileext = ".mat",
    compression = "none",
    verbose = verbose,
    timeout = timeout
  )
  format_isomap_swiss_roll(mat)
}

#' Download Isomap Faces
#'
#' Download the original Isomap face-pose dataset.
#'
#' The original Stanford URL for this dataset is no longer available, so the
#' default URL uses an archived copy from the Internet Archive. The archived
#' data is stored as a Unix `compress` file, and this function uses an
#' external `gzip` or `uncompress` command for decompression.
#'
#' @format A data frame with 698 rows and 4,098 variables:
#'
#' * `px1`, `px2`, `px3` ... `px4096`: Grayscale pixel
#'   values for a 64 x 64 image.
#' * `pose1`, `pose2`: Pose values supplied with the original
#'   dataset.
#'
#' @param url URL of the compressed Matlab data file.
#' @param verbose If `TRUE`, then download progress will be logged as a
#'   message.
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
#' @return Data frame containing the Isomap face-pose dataset.
#' @note Requires the
#' [R.matlab](https://cran.r-project.org/package=R.matlab)
#' package to be installed. The face data also requires an external
#' `gzip` or `uncompress` command that can decompress Unix
#' `compress` files.
#' @seealso Each row can be visualized using [show_isomap_face()].
#' @export
#' @examples
#' \dontrun{
#' faces <- download_isomap_faces()
#' show_isomap_face(faces, 1)
#' }
#' @references
#' Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000).
#' A global geometric framework for nonlinear dimensionality reduction.
#' *Science*, *290*(5500), 2319-2323.
download_isomap_faces <- function(
  url = isomap_faces_url,
  verbose = FALSE,
  timeout = 1800
) {
  mat <- read_isomap_mat_url(
    url = url,
    fileext = ".mat.Z",
    compression = "compress",
    verbose = verbose,
    timeout = timeout
  )
  format_isomap_faces(mat)
}

#' Visualize Isomap face.
#'
#' Display an image from the Isomap face-pose dataset.
#'
#' @param df Data frame containing Isomap faces, as returned by
#'   [download_isomap_faces()].
#' @param n Row index of the image to display.
#' @param col List of colors to use in the display.
#' @param ... Other arguments passed onto the [graphics::image()]
#'   function.
#' @export
show_isomap_face <- function(
  df,
  n,
  col = grDevices::gray(1:255 / 255),
  ...
) {
  if (
    !is.numeric(n) ||
      length(n) != 1 ||
      !is.finite(n) ||
      n < 1 ||
      n > nrow(df) ||
      n != floor(n)
  ) {
    stop("n must be an integer between 1 and ", nrow(df), call. = FALSE)
  }
  if (ncol(df) < 64 * 64) {
    stop("df must contain at least 4096 pixel columns", call. = FALSE)
  }

  pixels <- unlist(df[n, seq_len(64 * 64), drop = FALSE], use.names = FALSE)
  im <- matrix(as.numeric(pixels), nrow = 64, ncol = 64, byrow = TRUE)
  graphics::image(
    im[, 64:1],
    col = col,
    axes = FALSE,
    xlab = "",
    ylab = "",
    ...
  )
}

read_isomap_mat_url <- function(
  url,
  fileext,
  compression = c("none", "compress"),
  verbose = FALSE,
  timeout = 1800
) {
  compression <- match.arg(compression)
  stop_if_not_installed("R.matlab")

  downloaded_path <- tempfile(fileext = fileext)
  on.exit(cleanup_owned_paths(downloaded_path, verbose = verbose), add = TRUE)
  download_asset(url, downloaded_path, verbose = verbose, timeout = timeout)

  read_path <- downloaded_path
  if (compression == "compress") {
    decompressed_path <- decompress_isomap_compress(
      downloaded_path,
      verbose = verbose
    )
    on.exit(
      cleanup_owned_paths(decompressed_path, verbose = verbose),
      add = TRUE
    )
    read_path <- decompressed_path
  }

  read_isomap_mat(read_path)
}

read_isomap_mat <- function(path) {
  R.matlab::readMat(path)
}

decompress_isomap_compress <- function(path, verbose = FALSE) {
  decompressor <- isomap_compress_decompressor()
  out <- tempfile(fileext = ".mat")
  err <- tempfile()
  on.exit(unlink(err), add = TRUE)

  if (verbose) {
    message("Decompressing with ", decompressor$label)
    utils::flush.console()
  }

  status <- system2(
    decompressor$command,
    decompressor$args(path),
    stdout = out,
    stderr = err
  )
  if (status != 0) {
    unlink(out)
    err_msg <- readLines(err, warn = FALSE)
    stop(
      "Failed to decompress Unix compress file '",
      path,
      "'.\n",
      paste(err_msg, collapse = "\n"),
      call. = FALSE
    )
  }
  if (!file.exists(out) || file.info(out)$size == 0) {
    unlink(out)
    stop(
      "Failed to decompress Unix compress file '",
      path,
      "': decompressor produced no output",
      call. = FALSE
    )
  }

  out
}

isomap_compress_decompressor <- function() {
  gzip <- Sys.which("gzip")
  if (nzchar(gzip)) {
    return(list(
      command = unname(gzip),
      args = function(path) c("-dc", shQuote(path)),
      label = "gzip -dc"
    ))
  }

  uncompress <- Sys.which("uncompress")
  if (nzchar(uncompress)) {
    return(list(
      command = unname(uncompress),
      args = function(path) c("-c", shQuote(path)),
      label = "uncompress -c"
    ))
  }

  stop(
    "Decompressing Unix compress files requires an external ",
    "'gzip' or 'uncompress' command",
    call. = FALSE
  )
}

format_isomap_swiss_roll <- function(mat) {
  x <- isomap_mat_component(mat, "X.data", n_rows = 3)
  y <- isomap_mat_component(mat, "Y.data", n_rows = 2)

  if (ncol(x) != ncol(y)) {
    stop(
      "MAT file components 'X.data' and 'Y.data' have different column counts",
      call. = FALSE
    )
  }

  x <- as.data.frame(t(x))
  names(x) <- c("x", "y", "z")

  y <- as.data.frame(t(y))
  names(y) <- c("u", "v")

  data.frame(
    x,
    y,
    color = linear_color_map(y$u),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

format_isomap_faces <- function(mat) {
  images <- isomap_mat_component(mat, "images", n_rows = 64 * 64)
  poses <- isomap_mat_component(mat, "poses", n_rows = 2)

  if (ncol(images) != ncol(poses)) {
    stop(
      "MAT file components 'images' and 'poses' have different column counts",
      call. = FALSE
    )
  }

  pixels <- as.data.frame(t(images))
  names(pixels) <- paste0("px", seq_len(ncol(pixels)))

  poses <- as.data.frame(t(poses))
  names(poses) <- paste0("pose", seq_len(ncol(poses)))

  data.frame(pixels, poses, row.names = NULL)
}

isomap_mat_component <- function(mat, name, n_rows = NULL) {
  if (!name %in% names(mat)) {
    stop("MAT file does not contain component '", name, "'", call. = FALSE)
  }
  x <- mat[[name]]
  if (!is.matrix(x)) {
    stop("MAT file component '", name, "' must be a matrix", call. = FALSE)
  }
  if (!is.null(n_rows) && nrow(x) != n_rows) {
    stop(
      "MAT file component '",
      name,
      "' must have ",
      n_rows,
      " rows",
      call. = FALSE
    )
  }
  x
}
