# Map a vector x to a new linear scale in the range (from, to)
linear_map <- function(x, from = 0, to = 1) {
  if (!is.numeric(x)) {
    stop("x must be numeric", call. = FALSE)
  }
  if (!is.numeric(from) || length(from) != 1 || !is.finite(from)) {
    stop("from must be a finite numeric scalar", call. = FALSE)
  }
  if (!is.numeric(to) || length(to) != 1 || !is.finite(to)) {
    stop("to must be a finite numeric scalar", call. = FALSE)
  }
  if (length(x) == 0) {
    return(numeric())
  }
  if (any(!is.finite(x))) {
    stop("x must contain only finite values", call. = FALSE)
  }

  x_min <- min(x)
  x_range <- max(x) - x_min

  if (x_range == 0) {
    mapped <- rep((from + to) / 2, length(x))
    names(mapped) <- names(x)
    return(mapped)
  }

  (x - x_min) / x_range * (to - from) + from
}

# Linearly maps a numeric vector x onto a color scale ranging from
# hsl(0, s, l) to hsl(300, s, l) (basically a rainbow color scale).
linear_color_map <- function(x, h = 300, s = 50, l = 50) {
  vapply(
    linear_map(x, from = 0, to = 1),
    function(e) {
      hsl_to_rgb(h = floor(h * e), s = s, l = l)
    },
    ""
  )
}

# Helper function needed by hsl_to_rgb, from the CSS spec
# https://www.w3.org/TR/2011/REC-css3-color-20110607/#hsl-color
hue_to_rgb <- function(m1, m2, h) {
  if (h < 0) h <- h + 1
  if (h > 1) h <- h - 1
  if (h * 6 < 1) {
    return(m1 + (m2 - m1) * h * 6)
  }
  if (h * 2 < 1) {
    return(m2)
  }
  if (h * 3 < 2) {
    return(m1 + (m2 - m1) * (2 / 3 - h) * 6)
  }
  m1
}

# Converts HSL to RGB, using the algorithm specified by the CSS spec
# https://www.w3.org/TR/2011/REC-css3-color-20110607/#hsl-color
# h is the hue in degrees in the range (0, 360] (i.e. 360 is excluded).
# s is the saturation as % in the range (0, 100)
# l is the luminance as % in the range (0, 100).
# Returns an RGB string.
hsl_to_rgb <- function(h, s, l) {
  # Map degrees to range (0, 360)
  h <- (((h %% 360) + 360) %% 360) / 360
  s <- s / 100
  l <- l / 100

  if (s == 0) {
    # achromatic
    r <- g <- b <- l
  } else {
    if (l <= 0.5) {
      m2 <- l * (s + 1)
    } else {
      m2 <- l + s - l * s
    }
    m1 <- l * 2 - m2
    r <- hue_to_rgb(m1, m2, h + 1 / 3)
    g <- hue_to_rgb(m1, m2, h)
    b <- hue_to_rgb(m1, m2, h - 1 / 3)
  }
  grDevices::rgb(red = r, green = g, blue = b)
}

# Return a vector of n equally spaced angles from (0, 2*pi] radians
theta_unif <- function(n) {
  (0:(n - 1)) * (2 * pi / n)
}

# Replicate each row in df n times
# http://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
replicate_rows <- function(df, n) {
  df[rep(seq_len(nrow(df)), each = n), ]
}


# Split A Vector Into Equally Populated Factors
#
# Assigns each member of a vector to a factor, based on the quantiles of the
# distribution, so that each factor is equal populated. Levels range from
# one to \code{nfactors}.
#
# @param x Numeric vector
# @param nfactors Number of factors required.
# @return factor-encoded vector specifying the level for each item in the
# vector.
equal_factors <- function(x, nfactors) {
  breaks <- stats::quantile(x, probs = seq(0, 1, length.out = nfactors + 1))
  cuts <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  as.factor(cuts)
}


# Merge Two Data Frames By Row
#
# Merges two data frames in the style of an outer join.
#
# This function is intended to be used where two simulation sets of different
# dimensionality should be merged. Resulting NA values are set to 0. If the data
# frames have unequal numbers of columns, attempts to maintain the order of
# columns in the dataframe with the larger number of columns.
#
# x, y - Data frames to merge
# Returns a merged data frame.
merge_by_row <- function(x, y) {
  # keep column order of larger dataset
  if (ncol(x) < ncol(y)) {
    all_col_names <- union(names(y), names(x))
  } else {
    all_col_names <- union(names(x), names(y))
  }

  x[setdiff(all_col_names, names(x))] <- 0
  y[setdiff(all_col_names, names(y))] <- 0

  rbind(
    x[, all_col_names, drop = FALSE],
    y[, all_col_names, drop = FALSE]
  )
}

# Get the URL for the raw data version of a file on github
# e.g.
# https://raw.githubusercontent.com/PAIR-code/understanding-umap/master/raw_data/mammoth_3d.json
# becomes
# https://github.com/PAIR-code/understanding-umap/blob/master/raw_data/mammoth_3d.json
gh_raw <- function(repo, filename, branch = "master") {
  paste("https://raw.githubusercontent.com", repo, branch, filename, sep = "/")
}


is_installed <- function(pkgname) {
  requireNamespace(pkgname, quietly = TRUE)
  isNamespaceLoaded(pkgname)
}

stop_if_not_installed <- function(pkg) {
  if (!(is_installed(pkg))) {
    stop("Please install the '", pkg, "' package")
  }
}

estimate_matrix_bytes <- function(
  n_rows,
  n_cols,
  storage = c("integer", "double")
) {
  storage <- match.arg(storage)
  dimensions <- c(n_rows = n_rows, n_cols = n_cols)
  if (
    !is.numeric(dimensions) ||
      length(dimensions) != 2L ||
      any(!is.finite(dimensions)) ||
      any(dimensions < 0) ||
      any(dimensions != floor(dimensions))
  ) {
    stop(
      "Matrix dimensions must be non-negative finite integers",
      call. = FALSE
    )
  }

  bytes_per_value <- switch(storage, integer = 4, double = 8)
  bytes <- prod(dimensions) * bytes_per_value
  if (!is.finite(bytes)) {
    stop("Matrix byte estimate is too large", call. = FALSE)
  }
  bytes
}

format_memory_bytes <- function(bytes) {
  units <- c("B", "KiB", "MiB", "GiB", "TiB")
  unit <- 1L
  while (bytes >= 1024 && unit < length(units)) {
    bytes <- bytes / 1024
    unit <- unit + 1L
  }
  sprintf("%.2f %s", bytes, units[[unit]])
}

warn_wide_data_frame <- function(
  dataset,
  n_rows,
  n_cols,
  storage,
  as,
  threshold = 512 * 1024^2
) {
  bytes <- estimate_matrix_bytes(n_rows, n_cols, storage = storage)
  if (as == "data.frame" && bytes >= threshold) {
    warning(
      dataset,
      " requested with `as = \"data.frame\"` has ",
      n_rows,
      " rows and ",
      n_cols,
      " pixel columns. Its ",
      storage,
      " pixel matrix alone needs about ",
      format_memory_bytes(bytes),
      "; converting it to a wide data frame requires another large allocation. ",
      "Use `as = \"matrix\"` to avoid the wide data-frame conversion.",
      call. = FALSE
    )
  }
  invisible(bytes)
}

cleanup_owned_paths <- function(paths, verbose = FALSE) {
  for (path in unique(paths)) {
    if (file.exists(path) || dir.exists(path)) {
      if (verbose) {
        message("Deleting ", path)
      }
      unlink(path, recursive = TRUE)
    }
  }
}

with_download_timeout <- function(expr, timeout = 1800) {
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      !is.finite(timeout) ||
      timeout <= 0
  ) {
    stop("timeout must be a positive finite number of seconds", call. = FALSE)
  }

  old_options <- options("timeout")
  options(timeout = max(old_options$timeout, timeout))
  on.exit(options(old_options), add = TRUE)
  force(expr)
}

download_asset <- function(url, destfile, verbose = FALSE, timeout = 1800) {
  if (verbose) {
    message("Downloading ", url, " to ", destfile)
    utils::flush.console()
  }

  status <- with_download_timeout(
    utils::download.file(url, destfile, quiet = !verbose, mode = "wb"),
    timeout = timeout
  )
  if (status != 0 || !file.exists(destfile) || file.info(destfile)$size == 0) {
    stop("Failed to download ", url, " to ", destfile, call. = FALSE)
  }

  invisible(destfile)
}

normalize_tar_entry <- function(entry) {
  entry <- gsub("\\\\", "/", entry)
  entry <- sub("^\\./+", "", entry)
  entry <- sub("/+$", "", entry)

  if (entry == "" || grepl("^/", entry) || grepl("^[A-Za-z]:", entry)) {
    return(NA_character_)
  }

  parts <- strsplit(entry, "/+", perl = TRUE)[[1]]
  if (any(parts %in% c("", ".", ".."))) {
    return(NA_character_)
  }
  paste(parts, collapse = "/")
}

validate_tar_entries <- function(entries, asset) {
  normalized <- vapply(entries, normalize_tar_entry, character(1))
  unsafe <- is.na(normalized)
  if (any(unsafe)) {
    stop(
      asset,
      " contains unsafe archive paths: ",
      paste(entries[unsafe], collapse = ", "),
      call. = FALSE
    )
  }

  duplicates <- unique(normalized[duplicated(normalized)])
  if (length(duplicates) > 0L) {
    stop(
      asset,
      " contains duplicate archive paths: ",
      paste(duplicates, collapse = ", "),
      call. = FALSE
    )
  }

  normalized
}

tar_header_string <- function(bytes) {
  terminator <- match(as.raw(0), bytes)
  if (!is.na(terminator)) {
    bytes <- bytes[seq_len(terminator - 1L)]
  }
  if (length(bytes) == 0L) {
    return("")
  }
  rawToChar(bytes)
}

tar_header_size <- function(bytes, asset) {
  digits <- as.integer(bytes)
  digits <- digits[digits >= 48L & digits <= 55L]
  if (length(digits) == 0L) {
    return(0)
  }

  size <- 0
  for (digit in digits) {
    size <- size * 8 + digit - 48L
  }
  if (!is.finite(size)) {
    stop(asset, " contains a non-finite tar entry size", call. = FALSE)
  }
  size
}

validate_tar_entry_types <- function(tarfile, asset) {
  con <- gzfile(tarfile, "rb")
  on.exit(close(con), add = TRUE)

  repeat {
    header <- readBin(con, what = "raw", n = 512L)
    if (length(header) == 0L || all(header == as.raw(0))) {
      break
    }
    if (length(header) != 512L) {
      stop(asset, " has a truncated tar header", call. = FALSE)
    }

    path <- tar_header_string(header[seq_len(100L)])
    type <- rawToChar(header[[157L]])
    if (type %in% c("1", "2")) {
      stop(asset, " contains unsafe tar link entry: ", path, call. = FALSE)
    }
    if (!type %in% c("", "0", "5", "L", "K", "x", "g")) {
      stop(
        asset,
        " contains unsupported tar entry type '",
        type,
        "': ",
        path,
        call. = FALSE
      )
    }

    remaining_blocks <- ceiling(tar_header_size(header[125:136], asset) / 512)
    while (remaining_blocks > 0) {
      payload <- readBin(con, what = "raw", n = 512L)
      if (length(payload) != 512L) {
        stop(
          asset,
          " has a truncated tar payload for entry: ",
          path,
          call. = FALSE
        )
      }
      remaining_blocks <- remaining_blocks - 1
    }
  }
}

extract_tar_safely <- function(tarfile, exdir, asset, validate_layout) {
  validate_tar_entry_types(tarfile, asset)
  entries <- utils::untar(tarfile, list = TRUE, tar = "internal")
  if (length(entries) == 0L) {
    stop(asset, " contains no archive entries", call. = FALSE)
  }
  normalized <- validate_tar_entries(entries, asset)
  validate_layout(normalized, asset)
  utils::untar(tarfile, exdir = exdir, tar = "internal")
  invisible(normalized)
}

binary_header_context <- function(header = NULL) {
  if (is.null(header) || length(header) == 0L) {
    return("none")
  }

  if (is.null(names(header))) {
    names(header) <- paste0("value", seq_along(header))
  }

  paste(paste0(names(header), "=", header), collapse = ", ")
}

binary_byte_count <- function(count, size, asset, header = NULL) {
  if (
    length(count) != 1L ||
      !is.finite(count) ||
      count < 0 ||
      count != floor(count) ||
      count > .Machine$integer.max
  ) {
    stop(
      asset,
      " has an invalid read count: expected a nonnegative integer no larger than ",
      .Machine$integer.max,
      "; actual count ",
      count,
      "; header dimensions: ",
      binary_header_context(header),
      call. = FALSE
    )
  }

  count * size
}

read_binary_exact <- function(
  con,
  what,
  n,
  size,
  asset,
  component,
  signed = TRUE,
  endian = .Platform$endian,
  header = NULL
) {
  expected_bytes <- binary_byte_count(n, size, asset, header)
  values <- readBin(
    con,
    what = what,
    n = n,
    size = size,
    signed = signed,
    endian = endian
  )
  actual_count <- length(values)

  if (actual_count != n) {
    stop(
      asset,
      " has a truncated ",
      component,
      ": expected count ",
      n,
      " values (",
      expected_bytes,
      " bytes); actual count ",
      actual_count,
      " values; header dimensions: ",
      binary_header_context(header),
      call. = FALSE
    )
  }

  values
}

read_binary_scalar <- function(
  con,
  what,
  size,
  asset,
  component,
  signed = TRUE,
  endian = .Platform$endian,
  header = NULL
) {
  read_binary_exact(
    con = con,
    what = what,
    n = 1L,
    size = size,
    asset = asset,
    component = component,
    signed = signed,
    endian = endian,
    header = header
  )[[1]]
}

validate_binary_magic <- function(actual, expected, asset, header = NULL) {
  if (actual != expected) {
    stop(
      asset,
      " has an invalid magic number: expected ",
      expected,
      " (1 value / 4 bytes); actual ",
      actual,
      " (1 value / 4 bytes); header dimensions: ",
      binary_header_context(c(header, magic = actual)),
      call. = FALSE
    )
  }
}

validate_binary_dimensions <- function(dimensions, asset, header = dimensions) {
  invalid <-
    !is.numeric(dimensions) ||
    length(dimensions) == 0L ||
    any(!is.finite(dimensions)) ||
    any(dimensions <= 0) ||
    any(dimensions != floor(dimensions)) ||
    any(dimensions > .Machine$integer.max)

  if (invalid) {
    stop(
      asset,
      " has invalid dimensions: expected positive finite integer dimensions ",
      "with a payload count no larger than ",
      .Machine$integer.max,
      "; actual dimensions ",
      binary_header_context(header),
      "; expected payload count/bytes unavailable; actual payload count unavailable",
      call. = FALSE
    )
  }

  structure(as.integer(dimensions), names = names(dimensions))
}

binary_safe_product <- function(dimensions, asset, header = dimensions) {
  dimensions <- validate_binary_dimensions(dimensions, asset, header)
  count <- 1

  for (dimension in dimensions) {
    if (count > .Machine$integer.max / dimension) {
      stop(
        asset,
        " has an impossible payload size: expected count no larger than ",
        .Machine$integer.max,
        " values; actual declared count exceeds ",
        .Machine$integer.max,
        " values; header dimensions: ",
        binary_header_context(header),
        call. = FALSE
      )
    }
    count <- count * dimension
  }

  as.integer(count)
}

assert_binary_eof <- function(con, asset, header = NULL) {
  trailing <- readBin(con, what = "raw", n = 1L)

  if (length(trailing) != 0L) {
    stop(
      asset,
      " has trailing data: expected count 0 values (0 bytes); actual count ",
      length(trailing),
      " values (",
      length(trailing),
      " bytes); header dimensions: ",
      binary_header_context(header),
      call. = FALSE
    )
  }
}

stime <- function() {
  format(Sys.time(), "%T")
}

tsmessage <- function(
  ...,
  domain = NULL,
  appendLF = TRUE,
  force = FALSE,
  time_stamp = TRUE
) {
  verbose <- get0("verbose", envir = sys.parent())

  if (force || (!is.null(verbose) && verbose)) {
    msg <- ""
    if (time_stamp) {
      msg <- paste0(stime(), " ")
    }
    message(msg, ..., domain = domain, appendLF = appendLF)
    utils::flush.console()
  }
}
