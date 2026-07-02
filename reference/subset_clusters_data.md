# Subset Cluster Data

One tiny gaussian cluster inside of a big cluster from "How to Use t-SNE
Effectively".

## Usage

``` r
subset_clusters_data(n, dim = 2, big_sdev = 50)
```

## Arguments

- n:

  Number of points per gaussian.

- dim:

  Dimension of the gaussians.

- big_sdev:

  Standard deviation of the bigger cluster, default 50. The smaller
  cluster has a standard deviation of 1.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xdim` columns, and
color in the `color` column.

## Details

Creates a dataset consisting of two gaussians with the same center, but
with the first cluster having a standard deviation of 1, and the second
having a standard deviation of `big_sdev` (default 50). Points are
colored depending on which cluster they belong to (small cluster is dark
powder blue, large is light orange).

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
[`three_clusters_data()`](https://jlmelville.github.io/snedata/reference/three_clusters_data.md),
[`trefoil_data()`](https://jlmelville.github.io/snedata/reference/trefoil_data.md),
[`two_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_clusters_data.md),
[`two_different_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_different_clusters_data.md),
[`unlink_data()`](https://jlmelville.github.io/snedata/reference/unlink_data.md)

## Examples

``` r
df <- subset_clusters_data(n = 50, dim = 2)

# 10D example where the big cluster is only twice the standard deviation of
# the small cluster
df <- subset_clusters_data(n = 50, dim = 10, big_sdev = 2)
```
