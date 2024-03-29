# Map a vector x to a new linear scale in the range (from, to)
linear_map <- function(x, from = 0, to = 1) {
  (x - min(x)) / max(x - min(x)) * (to - from) + from
}

# Linearly maps a numeric vector x onto a color scale ranging from
# hsl(0, s, l) to hsl(300, s, l) (basically a rainbow color scale).
linear_color_map <- function(x, h = 300, s = 50, l = 50) {
  vapply(
    linear_map(x, from = 0, to = 1),
    function(e) {
      hsl_to_rgb(h = floor(h * e), s = s, l = l)
    }, ""
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
  z <- merge(x, y, all = TRUE, sort = FALSE)

  # keep column order of larger dataset
  if (ncol(x) < ncol(y)) {
    all_col_names <- union(names(y), names(x))
  } else {
    all_col_names <- union(names(x), names(y))
  }
  z <- z[, all_col_names]

  z[is.na(z)] <- 0

  z
}

# Get the URL for the raw data version of a file on github
# e.g.
# https://raw.githubusercontent.com/PAIR-code/understanding-umap/master/raw_data/mammoth_3d.json
# becomes
# https://github.com/PAIR-code/understanding-umap/blob/master/raw_data/mammoth_3d.json
gh_raw <- function(repo, filename, branch = "master") {
  paste("https://raw.githubusercontent.com",
    repo,
    branch,
    filename,
    sep = "/"
  )
}

stop_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg,
    quietly = TRUE,
    warn.conflicts = FALSE
  )) {
    stop("Please install the '", pkg, "' package")
  }
}

stime <- function() {
  format(Sys.time(), "%T")
}

tsmessage <- function(...,
                      domain = NULL,
                      appendLF = TRUE,
                      force = FALSE,
                      time_stamp = TRUE) {
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
