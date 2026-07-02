# Two Gaussian Clusters With Unequal Standard Deviations

Two gaussians with equal size but unequal bandwidths, from "How to Use
t-SNE Effectively".

## Usage

``` r
two_different_clusters_data(n, dim = 50, scale = 10)
```

## Arguments

- n:

  Number of points per gaussian.

- dim:

  Dimension of the gaussians.

- scale:

  Amount to reduce the standard deviation of the second cluster,
  relative to the first.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns, and
color in the `color` column.

## Details

Creates a dataset consisting of two symmetric gaussian distributions
with equal number of points, but different standard deviations: the
standard deviations of the second cluster will be `1/scale` of the
other. Clusters are separated by 20 units. Points are colored depending
on which cluster they belong to.

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
[`long_gaussian_data()`](https://jlmelville.github.io/snedata/reference/long_gaussian_data.md),
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
[`unlink_data()`](https://jlmelville.github.io/snedata/reference/unlink_data.md)

## Examples

``` r
df <- two_different_clusters_data(n = 50, dim = 2, scale = 5)
```
