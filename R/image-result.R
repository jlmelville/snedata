#' Canonical image-dataset results
#'
#' Image downloaders accept `as = "list"` to return one shared, shallow
#' structure. It has `data`, a matrix with one image per row; `meta`, a data
#' frame with matching rows; `image_dim`, named image dimensions; `channel_order`;
#' and `source`, a list containing the dataset name and acquisition URL.
#'
#' Metadata names are lower case: `label`, `description`, `split`, `id`,
#' `object`, and `pose` are used when applicable. Dataset-specific source fields
#' remain in `meta`. `split` records train/test identity explicitly. The legacy
#' data-frame result remains available with `as = "data.frame"` and retains its
#' historical column names.
#'
#' @name image-results
#' @keywords datasets
NULL

# Canonical image-dataset result helpers.

image_result_as <- function(as) {
  match.arg(as, c("data.frame", "list"))
}

new_image_result <- function(data, meta, image_dim, channel_order, source) {
  if (!is.matrix(data)) {
    stop("`data` must be a matrix", call. = FALSE)
  }
  if (!is.data.frame(meta) || nrow(meta) != nrow(data)) {
    stop("`meta` must be a data frame with one row per image", call. = FALSE)
  }
  if (
    !is.numeric(image_dim) ||
      length(image_dim) < 2L ||
      any(!is.finite(image_dim)) ||
      any(image_dim <= 0)
  ) {
    stop("`image_dim` must contain positive image dimensions", call. = FALSE)
  }
  if (is.null(names(image_dim))) {
    stop("`image_dim` must have named dimensions", call. = FALSE)
  }
  if (!is.character(channel_order) || length(channel_order) < 1L) {
    stop("`channel_order` must name one or more channels", call. = FALSE)
  }
  if (!is.list(source) || is.null(source$dataset) || is.null(source$url)) {
    stop("`source` must include `dataset` and `url`", call. = FALSE)
  }

  list(
    data = data,
    meta = meta,
    image_dim = structure(as.integer(image_dim), names = names(image_dim)),
    channel_order = channel_order,
    source = source
  )
}

image_result_data <- function(x) {
  if (is.list(x) && !is.null(x$data) && !is.null(x$meta)) {
    return(x$data)
  }
  if (is.data.frame(x) || is.matrix(x)) {
    return(x)
  }
  stop(
    "`x` must be a canonical image result, data frame, or matrix",
    call. = FALSE
  )
}

image_result_meta <- function(x) {
  if (is.list(x) && !is.null(x$data) && !is.null(x$meta)) {
    return(x$meta)
  }
  NULL
}
