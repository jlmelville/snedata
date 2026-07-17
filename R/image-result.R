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
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("`data` must be a numeric matrix", call. = FALSE)
  }
  if (!is.data.frame(meta) || nrow(meta) != nrow(data)) {
    stop("`meta` must be a data frame with one row per image", call. = FALSE)
  }
  if (
    !is.numeric(image_dim) ||
      length(image_dim) < 2L ||
      any(!is.finite(image_dim)) ||
      any(image_dim <= 0) ||
      any(image_dim != floor(image_dim)) ||
      any(image_dim > .Machine$integer.max)
  ) {
    stop(
      "`image_dim` must contain positive integer image dimensions",
      call. = FALSE
    )
  }
  image_dim_names <- names(image_dim)
  if (
    is.null(image_dim_names) ||
      anyNA(image_dim_names) ||
      any(!nzchar(image_dim_names)) ||
      anyDuplicated(image_dim_names)
  ) {
    stop(
      "`image_dim` must have unique, nonempty dimension names",
      call. = FALSE
    )
  }
  image_dim <- structure(as.integer(image_dim), names = image_dim_names)
  n_pixels <- image_dimension_product(image_dim)
  if (n_pixels != ncol(data)) {
    stop(
      "`image_dim` describes ",
      n_pixels,
      " pixels but `data` has ",
      ncol(data),
      " columns",
      call. = FALSE
    )
  }
  if (
    !is.character(channel_order) ||
      length(channel_order) < 1L ||
      anyNA(channel_order) ||
      any(!nzchar(channel_order)) ||
      anyDuplicated(channel_order)
  ) {
    stop(
      "`channel_order` must contain unique, nonempty channel names",
      call. = FALSE
    )
  }
  channel_dimension <- intersect(c("channels", "cameras"), image_dim_names)
  if (length(channel_dimension) > 1L) {
    stop(
      "`image_dim` cannot declare both `channels` and `cameras`",
      call. = FALSE
    )
  }
  expected_channels <- if (length(channel_dimension) == 1L) {
    image_dim[[channel_dimension]]
  } else {
    1L
  }
  if (length(channel_order) != expected_channels) {
    stop(
      "`channel_order` has ",
      length(channel_order),
      " names but `image_dim` declares ",
      expected_channels,
      " channels or cameras",
      call. = FALSE
    )
  }
  validate_image_source(source)

  list(
    data = data,
    meta = meta,
    image_dim = image_dim,
    channel_order = channel_order,
    source = source
  )
}

image_dimension_product <- function(image_dim) {
  n_pixels <- 1L
  for (dimension in image_dim) {
    if (n_pixels > .Machine$integer.max / dimension) {
      stop(
        "`image_dim` describes more than ",
        .Machine$integer.max,
        " matrix columns",
        call. = FALSE
      )
    }
    n_pixels <- n_pixels * dimension
  }
  as.integer(n_pixels)
}

validate_image_source <- function(source) {
  if (!is.list(source)) {
    stop(
      "`source` must be a list containing `dataset` and `url`",
      call. = FALSE
    )
  }
  for (field in c("dataset", "url")) {
    value <- source[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      stop(
        "`source$",
        field,
        "` must be a nonempty character scalar",
        call. = FALSE
      )
    }
  }
  invisible(source)
}

stop_dataset_field <- function(dataset, split, field, allowed, observed) {
  if (length(observed) == 0L) observed <- "<none>"
  stop(
    dataset,
    " ",
    split,
    " field '",
    field,
    "' is invalid: allowed ",
    paste(allowed, collapse = ", "),
    "; observed ",
    paste(observed, collapse = ", "),
    call. = FALSE
  )
}

validate_dataset_values <- function(values, allowed, dataset, split, field) {
  values_character <- as.character(values)
  allowed_character <- as.character(allowed)
  invalid <- is.na(values_character) |
    !(values_character %in% allowed_character)
  if (any(invalid)) {
    observed <- unique(values_character[invalid])
    observed[is.na(observed)] <- "<NA>"
    stop_dataset_field(
      dataset,
      split,
      field,
      allowed = allowed_character,
      observed = observed
    )
  }
  invisible(values)
}

validate_image_result_compatibility <- function(train, test) {
  if (ncol(train$data) != ncol(test$data)) {
    stop("Training and testing image column counts must match", call. = FALSE)
  }
  if (!identical(train$image_dim, test$image_dim)) {
    stop("Training and testing `image_dim` values must match", call. = FALSE)
  }
  if (!identical(train$channel_order, test$channel_order)) {
    stop(
      "Training and testing `channel_order` values must match",
      call. = FALSE
    )
  }
  if (!identical(train$source$dataset, test$source$dataset)) {
    stop("Training and testing source datasets must match", call. = FALSE)
  }
  invisible(NULL)
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
