# Elongated Gaussian Ellipsoid

Elongated gaussian data from "How to Use t-SNE Effectively".

## Usage

``` r
long_gaussian_data(n, dim, color = NULL)
```

## Arguments

- n:

  Number of points.

- dim:

  Dimension of the gaussian.

- color:

  Color to assign to the data points.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns, and
color in the `color` column.

## Details

Creates a randomly-sampled gaussian dataset of the specified dimension,
with standard deviation following the harmonic series: the first column
has standard deviation 1, the second 1/2, the third 1/3 and so on.

## References

<http://distill.pub/2016/misread-tsne/>

## See also

Other distill functions:
[`circle_data()`](https://jlmelville.github.io/snedata/reference/circle_data.md),
[`cube_data()`](https://jlmelville.github.io/snedata/reference/cube_data.md),
[`gaussian_data()`](https://jlmelville.github.io/snedata/reference/gaussian_data.md),
[`grid_data()`](https://jlmelville.github.io/snedata/reference/grid_data.md),
[`link_data()`](https://jlmelville.github.io/snedata/reference/link_data.md),
[`long_cluster_data()`](https://jlmelville.github.io/snedata/reference/long_cluster_data.md),
[`ortho_curve()`](https://jlmelville.github.io/snedata/reference/ortho_curve.md),
[`random_circle_cluster_data()`](https://jlmelville.github.io/snedata/reference/random_circle_cluster_data.md),
[`random_circle_data()`](https://jlmelville.github.io/snedata/reference/random_circle_data.md),
[`random_jump()`](https://jlmelville.github.io/snedata/reference/random_jump.md),
[`random_walk()`](https://jlmelville.github.io/snedata/reference/random_walk.md),
[`simplex_data()`](https://jlmelville.github.io/snedata/reference/simplex_data.md),
[`subset_clusters_data()`](https://jlmelville.github.io/snedata/reference/subset_clusters_data.md),
[`three_clusters_data()`](https://jlmelville.github.io/snedata/reference/three_clusters_data.md),
[`trefoil_data()`](https://jlmelville.github.io/snedata/reference/trefoil_data.md),
[`two_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_clusters_data.md),
[`two_different_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_different_clusters_data.md),
[`unlink_data()`](https://jlmelville.github.io/snedata/reference/unlink_data.md)

## Examples

``` r
df <- long_gaussian_data(n = 50, dim = 2)
```
