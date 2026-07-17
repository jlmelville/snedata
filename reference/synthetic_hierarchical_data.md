# Synthetic Hierarchical Data Set

Simulation data from a hierarchy of Gaussian clusters.

## Usage

``` r
synthetic_hierarchical_data(
  n = 500,
  dim = 50,
  colors = c("full", "macro", "none")
)
```

## Arguments

- n:

  Number of observations to sample from each micro cluster.

- dim:

  Dimension of the Gaussian observations and cluster centers.

- colors:

  Color detail to include: `"full"` (the default) adds macro, meso, and
  micro plotting colors and requires the optional
  [colorspace](https://cran.r-project.org/package=colorspace) package;
  `"macro"` adds a dependency-free anchor color for each macro cluster;
  and `"none"` returns coordinates and labels only.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns, and
factor columns `macro_label`, `meso_label`, and `micro_label`.
`colors = "macro"` also includes `color` and `macro_color`;
`colors = "full"` also includes `color`, `macro_color`, `meso_color`,
and `micro_color` plotting columns.

## Details

Creates the synthetic hierarchical data set described by Wang and
co-workers (2021): five macro clusters, each containing five meso
clusters, each containing five micro clusters. By default each micro
cluster contains 500 observations in 50 dimensions, for 62,500
observations in total.

Macro cluster centers are sampled from a zero-centered multivariate
normal distribution with diagonal covariance 10000. Meso cluster centers
are sampled around their macro center with diagonal covariance 1000.
Micro cluster centers are sampled around their meso center with diagonal
covariance 100. Observations are sampled around their micro center with
diagonal covariance 10.

This is based on a Python notebook which can be found at
<https://github.com/jlmelville/drnb/blob/d542c97b26d6f0f481d7551b130c41fcc0206c6b/notebooks/data-pipeline/synthetic-hierarchical.ipynb>

## References

Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021). Understanding
how dimension reduction tools work: an empirical approach to deciphering
t-SNE, UMAP, TriMAP, and PaCMAP for data visualization. *J Mach. Learn.
Res*, *22*, 1-73.

## See also

The [gitub repo](https://github.com/hyhuang00/scRNA-DR2020/) for the
paper ["Towards a comprehensive evaluation of dimension reduction
methods for transcriptomic data
visualization"](https://doi.org/10.1038/s42003-022-03628-x), which
contains a numpy-formatted version of data generated under a similar
distribution, under the name `hierarchical_threelayer_dataset`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- synthetic_hierarchical_data()
plot(df$X1, df$X2, col = df$color, pch = 20)
} # }
```
