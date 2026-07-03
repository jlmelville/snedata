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
#' @return Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns,
#'   factor columns `macro_label`, `meso_label`, and `micro_label`, a `color`
#'   column containing the meso-level color, and `macro_color`, `meso_color`,
#'   and `micro_color` columns for plotting at each hierarchy level.
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
synthetic_hierarchical_data <- function(n = 500, dim = 50) {
  n <- positive_integer_scalar(n, "n")
  dim <- positive_integer_scalar(dim, "dim")

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

  macro_levels <- as.character(0:(n_macro - 1L))
  meso_levels <- unlist(
    lapply(macro_levels, function(macro) {
      paste(macro, 0:(n_meso - 1L), sep = "_")
    }),
    use.names = FALSE
  )
  micro_levels <- unlist(
    lapply(meso_levels, function(meso) {
      paste(meso, 0:(n_micro - 1L), sep = "_")
    }),
    use.names = FALSE
  )

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

  color_maps <- synthetic_hierarchical_color_maps()
  macro_color <- unname(color_maps$macro[macro_labels])
  meso_color <- unname(color_maps$meso[meso_labels])
  micro_color <- unname(color_maps$micro[micro_labels])

  data.frame(
    x,
    macro_label = factor(macro_labels, levels = macro_levels),
    meso_label = factor(meso_labels, levels = meso_levels),
    micro_label = factor(micro_labels, levels = micro_levels),
    color = meso_color,
    macro_color = macro_color,
    meso_color = meso_color,
    micro_color = micro_color,
    stringsAsFactors = FALSE
  )
}

# Colors are hard-coded here and were generated by the Python package glasbey,
# <https://github.com/lmcinnes/glasbey>. See the python notebook for more
# details.
synthetic_hierarchical_color_maps <- function() {
  micro_color <- c(
    "0_0_0" = "#56000c",
    "0_0_1" = "#600010",
    "0_0_2" = "#6c0016",
    "0_0_3" = "#76001a",
    "0_0_4" = "#810020",
    "0_1_0" = "#8d0024",
    "0_1_1" = "#990028",
    "0_1_2" = "#a3002c",
    "0_1_3" = "#af0030",
    "0_1_4" = "#b90c36",
    "0_2_0" = "#c31a3a",
    "0_2_1" = "#cd243e",
    "0_2_2" = "#d53244",
    "0_2_3" = "#db4048",
    "0_2_4" = "#e14c4e",
    "0_3_0" = "#e55652",
    "0_3_1" = "#e96258",
    "0_3_2" = "#ed6c5c",
    "0_3_3" = "#f17862",
    "0_3_4" = "#f37e64",
    "0_4_0" = "#f78b6a",
    "0_4_1" = "#f9956e",
    "0_4_2" = "#fb9f72",
    "0_4_3" = "#fda978",
    "0_4_4" = "#fdb37c",
    "1_0_0" = "#02009b",
    "1_0_1" = "#0400af",
    "1_0_2" = "#0a00c1",
    "1_0_3" = "#1000d3",
    "1_0_4" = "#161cd9",
    "1_1_0" = "#182add",
    "1_1_1" = "#163ae3",
    "1_1_2" = "#1646e9",
    "1_1_3" = "#1e52f1",
    "1_1_4" = "#1e5cf7",
    "1_2_0" = "#2266fd",
    "1_2_1" = "#2a70fd",
    "1_2_2" = "#347cf9",
    "1_2_3" = "#4087f7",
    "1_2_4" = "#488df5",
    "1_3_0" = "#5497f3",
    "1_3_1" = "#5e9ff3",
    "1_3_2" = "#6aa9f3",
    "1_3_3" = "#74b1f1",
    "1_3_4" = "#7eb9f3",
    "1_4_0" = "#89c1f3",
    "1_4_1" = "#93c9f3",
    "1_4_2" = "#9dd1f5",
    "1_4_3" = "#a7d9f7",
    "1_4_4" = "#afe1f9",
    "2_0_0" = "#002c00",
    "2_0_1" = "#003000",
    "2_0_2" = "#003600",
    "2_0_3" = "#004000",
    "2_0_4" = "#004800",
    "2_1_0" = "#005000",
    "2_1_1" = "#005a00",
    "2_1_2" = "#006000",
    "2_1_3" = "#006a00",
    "2_1_4" = "#007000",
    "2_2_0" = "#0e7a02",
    "2_2_1" = "#1e7e02",
    "2_2_2" = "#2c8706",
    "2_2_3" = "#388f08",
    "2_2_4" = "#42970c",
    "2_3_0" = "#4c9f10",
    "2_3_1" = "#54a512",
    "2_3_2" = "#60ad16",
    "2_3_3" = "#68b318",
    "2_3_4" = "#74bb1a",
    "2_4_0" = "#7cc31e",
    "2_4_1" = "#89cb20",
    "2_4_2" = "#95d524",
    "2_4_3" = "#9fdd26",
    "2_4_4" = "#ade72a",
    "3_0_0" = "#540666",
    "3_0_1" = "#600a74",
    "3_0_2" = "#6a0e81",
    "3_0_3" = "#76108d",
    "3_0_4" = "#811499",
    "3_1_0" = "#8b16a5",
    "3_1_1" = "#951aaf",
    "3_1_2" = "#9f20bb",
    "3_1_3" = "#a726c3",
    "3_1_4" = "#b32ad1",
    "3_2_0" = "#bb32d9",
    "3_2_1" = "#c738e5",
    "3_2_2" = "#d142ef",
    "3_2_3" = "#db4cf9",
    "3_2_4" = "#e358ff",
    "3_3_0" = "#e968ff",
    "3_3_1" = "#ef76ff",
    "3_3_2" = "#f583ff",
    "3_3_3" = "#f991ff",
    "3_3_4" = "#fb9dff",
    "3_4_0" = "#fda9ff",
    "3_4_1" = "#ffb5ff",
    "3_4_2" = "#ffbfff",
    "3_4_3" = "#ffcbff",
    "3_4_4" = "#ffd3ff",
    "4_0_0" = "#2a2426",
    "4_0_1" = "#2e2a2c",
    "4_0_2" = "#343034",
    "4_0_3" = "#3c383c",
    "4_0_4" = "#403e42",
    "4_1_0" = "#48464a",
    "4_1_1" = "#4e4c50",
    "4_1_2" = "#585458",
    "4_1_3" = "#605c60",
    "4_1_4" = "#666264",
    "4_2_0" = "#6e6a6c",
    "4_2_1" = "#767272",
    "4_2_2" = "#7c7876",
    "4_2_3" = "#87817c",
    "4_2_4" = "#8f8981",
    "4_3_0" = "#978f85",
    "4_3_1" = "#9f9789",
    "4_3_2" = "#a79d8b",
    "4_3_3" = "#afa591",
    "4_3_4" = "#b9ad93",
    "4_4_0" = "#c1b395",
    "4_4_1" = "#cbbd99",
    "4_4_2" = "#d5c59b",
    "4_4_3" = "#e1cf9f",
    "4_4_4" = "#ebd7a1"
  )

  meso_color <- micro_color[endsWith(names(micro_color), "_2")]
  names(meso_color) <- sub("_[0-4]$", "", names(meso_color))

  macro_color <- meso_color[endsWith(names(meso_color), "_2")]
  names(macro_color) <- sub("_[0-4]$", "", names(macro_color))

  list(
    macro = macro_color,
    meso = meso_color,
    micro = micro_color
  )
}
