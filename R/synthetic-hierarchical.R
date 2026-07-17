#' Synthetic Hierarchical Data Set
#'
#' Simulation data from a hierarchy of Gaussian clusters.
#'
#' Creates the synthetic hierarchical data set described by Wang and co-workers
#' (2021): five macro clusters, each containing five meso clusters, each
#' containing five micro clusters. By default each micro cluster contains 500
#' observations in 50 dimensions, for 62,500 observations in total.
#'
#' Macro cluster centers are sampled from a zero-centered multivariate normal
#' distribution with diagonal covariance 10000. Meso cluster centers are sampled
#' around their macro center with diagonal covariance 1000. Micro cluster
#' centers are sampled around their meso center with diagonal covariance 100.
#' Observations are sampled around their micro center with diagonal covariance
#' 10.
#'
#' This is based on a Python notebook which can be found at
#' <https://github.com/jlmelville/drnb/blob/d542c97b26d6f0f481d7551b130c41fcc0206c6b/notebooks/data-pipeline/synthetic-hierarchical.ipynb>
#'
#' @param n Number of observations to sample from each micro cluster.
#' @param dim Dimension of the Gaussian observations and cluster centers.
#' @param colors Color detail to include: `"full"` (the default) adds macro,
#'   meso, and micro plotting colors and requires the optional
#'   [colorspace](https://cran.r-project.org/package=colorspace) package;
#'   `"macro"` adds a dependency-free anchor color for each macro cluster; and
#'   `"none"` returns coordinates and labels only.
#' @return Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns,
#'   and factor columns `macro_label`, `meso_label`, and `micro_label`.
#'   `colors = "macro"` also includes `color` and `macro_color`; `colors =
#'   "full"` also includes `color`, `macro_color`, `meso_color`, and
#'   `micro_color` plotting columns.
#' @references
#' Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021).
#' Understanding how dimension reduction tools work: an empirical approach to
#' deciphering t-SNE, UMAP, TriMAP, and PaCMAP for data visualization.
#' *J Mach. Learn. Res*, *22*, 1-73.
#' @seealso The [gitub repo](https://github.com/hyhuang00/scRNA-DR2020/) for
#'   the paper ["Towards a comprehensive evaluation of dimension reduction methods for transcriptomic data visualization"](https://doi.org/10.1038/s42003-022-03628-x),
#'   which contains a numpy-formatted version of data generated under a similar
#'   distribution, under the name `hierarchical_threelayer_dataset`.
#'
#' @examples
#' \dontrun{
#' df <- synthetic_hierarchical_data()
#' plot(df$X1, df$X2, col = df$color, pch = 20)
#' }
#' @export
synthetic_hierarchical_data <- function(
  n = 500,
  dim = 50,
  colors = c("full", "macro", "none")
) {
  n <- positive_integer_scalar(n, "n")
  dim <- positive_integer_scalar(dim, "dim")
  colors <- match.arg(colors)
  if (colors == "full") stop_if_not_installed("colorspace")

  n_macro <- 5L
  n_meso <- 5L
  n_micro <- 5L

  macro_var <- 10000
  meso_var <- 1000
  micro_var <- 100
  obs_var <- 10

  rnorm_centered <- function(nrow, center, variance) {
    x <- matrix(
      stats::rnorm(nrow * dim, sd = sqrt(variance)),
      nrow = nrow,
      ncol = dim
    )
    sweep(x, 2, center, `+`)
  }

  # labels are <macro_idx>_<meso_idx>_<micro_idx>, e.g. "0_0_0", "0_0_1", ...
  macro_levels <- as.character(0:(n_macro - 1L))
  meso_levels <- hierarchical_label_levels(macro_levels, n_meso)
  micro_levels <- hierarchical_label_levels(meso_levels, n_micro)

  total_n <- length(micro_levels) * n
  x <- matrix(NA_real_, nrow = total_n, ncol = dim)
  macro_labels <- character(total_n)
  meso_labels <- character(total_n)
  micro_labels <- character(total_n)

  macro_cluster_centers <- rnorm_centered(
    nrow = n_macro,
    center = rep(0, dim),
    variance = macro_var
  )

  row_start <- 1L
  for (macro_idx in seq_len(n_macro)) {
    macro_label <- as.character(macro_idx - 1L)
    macro_center <- macro_cluster_centers[macro_idx, ]
    meso_cluster_centers <- rnorm_centered(
      nrow = n_meso,
      center = macro_center,
      variance = meso_var
    )

    for (meso_idx in seq_len(n_meso)) {
      meso_label <- paste(macro_label, meso_idx - 1L, sep = "_")
      meso_center <- meso_cluster_centers[meso_idx, ]
      micro_cluster_centers <- rnorm_centered(
        nrow = n_micro,
        center = meso_center,
        variance = micro_var
      )

      for (micro_idx in seq_len(n_micro)) {
        micro_label <- paste(meso_label, micro_idx - 1L, sep = "_")
        rows <- row_start:(row_start + n - 1L)
        x[rows, ] <- rnorm_centered(
          nrow = n,
          center = micro_cluster_centers[micro_idx, ],
          variance = obs_var
        )
        macro_labels[rows] <- macro_label
        meso_labels[rows] <- meso_label
        micro_labels[rows] <- micro_label
        row_start <- row_start + n
      }
    }
  }

  result <- data.frame(
    x,
    macro_label = factor(macro_labels, levels = macro_levels),
    meso_label = factor(meso_labels, levels = meso_levels),
    micro_label = factor(micro_labels, levels = micro_levels),
    stringsAsFactors = FALSE
  )
  # roughly the colors used in the PaCMAP paper, specific hex codes are from
  # Set1 in RColorBrewer (but pointless to require that as a dependency just
  # for this one).
  anchors <- c("#377EB8", "#E41A1C", "#4DAF4A", "#A65628", "#999999")
  macro_color <- unname(stats::setNames(anchors, macro_levels)[macro_labels])

  if (colors == "none") return(result)
  if (colors == "macro") {
    return(
      data.frame(
        result,
        color = macro_color,
        macro_color = macro_color,
        stringsAsFactors = FALSE
      )
    )
  }

  color_maps <- synthetic_hierarchical_color_maps(
    n_macro = n_macro,
    n_meso = n_meso,
    n_micro = n_micro,
    anchors = anchors
  )
  macro_color <- unname(color_maps$macro[macro_labels])
  meso_color <- unname(color_maps$meso[meso_labels])
  micro_color <- unname(color_maps$micro[micro_labels])

  data.frame(
    result,
    color = meso_color,
    macro_color = macro_color,
    meso_color = meso_color,
    micro_color = micro_color,
    stringsAsFactors = FALSE
  )
}

# generate labels for a child: appends `_0` ... `_n_child-1` to each parent
# label, e.g. hierarchical_label_levels("2", 3) -> "2_0", "2_1", "2_2"
# or hierarchical_label_levels("2_1", 2) -> "2_1_0", "2_1_1"
hierarchical_label_levels <- function(parent_levels, n_child) {
  unlist(
    lapply(parent_levels, function(parent) {
      paste(parent, seq_len(n_child) - 1L, sep = "_")
    }),
    use.names = FALSE
  )
}

synthetic_hierarchical_color_maps <- function(
  n_macro,
  n_meso,
  n_micro,
  anchors
) {
  macro_levels <- as.character(seq_len(n_macro) - 1L)
  meso_levels <- hierarchical_label_levels(macro_levels, n_meso)
  micro_levels <- hierarchical_label_levels(meso_levels, n_micro)

  micro_color <- grouped_palette(
    rep(n_meso * n_micro, n_macro),
    anchors = anchors
  )
  names(micro_color) <- micro_levels

  meso_middle_micro <- paste(meso_levels, n_micro %/% 2L, sep = "_")
  meso_color <- micro_color[meso_middle_micro]
  names(meso_color) <- meso_levels

  anchor_rgb <- grDevices::col2rgb(anchors)
  macro_color <- grDevices::rgb(
    anchor_rgb["red", ],
    anchor_rgb["green", ],
    anchor_rgb["blue", ],
    maxColorValue = 255
  )
  names(macro_color) <- macro_levels

  list(
    macro = macro_color,
    meso = meso_color,
    micro = micro_color
  )
}

# an approximate implementation of the python grouped_palette function from
# glasbey. Not intended for general use, but you can see how you could get
# there if wanted. The purpose here is to provide each micro cluster with a
# color that is related to the meso and macro cluster it belongs to, with a
# separate color for each macro cluster
# A more generic function would allow anchors = NULL and then use
# qualpalr::qualpal() to generate the anchor colors, but we hardcode the
# choice of colors in synthetic_hierarchical_data, so this saves on a
# dependency.
grouped_palette <- function(
  block_sizes,
  anchors,
  within_c = c(75, 35),
  within_l = c(35, 85),
  neutral_threshold = 8,
  power = 1
) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("grouped_palette() requires the colorspace package", call. = FALSE)
  }
  block_sizes <- as.integer(block_sizes)

  power <- rep(power, length.out = 2L)
  anchor_rgb <- grDevices::col2rgb(anchors)
  anchors <- grDevices::rgb(
    anchor_rgb["red", ],
    anchor_rgb["green", ],
    anchor_rgb["blue", ],
    maxColorValue = 255
  )
  anchor_hcl <- colorspace::coords(
    methods::as(colorspace::hex2RGB(anchors), "polarLUV")
  )

  unlist(
    Map(
      function(n, h, c) {
        c_values <- if (c <= neutral_threshold) c(0, 0) else within_c
        colorspace::sequential_hcl(
          n,
          h = h,
          c = c_values,
          l = within_l,
          power = power
        )
      },
      block_sizes,
      anchor_hcl[, "H"],
      anchor_hcl[, "C"]
    ),
    use.names = FALSE
  )
}
