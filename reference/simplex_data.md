# Simplex Data

Data in a rough simplex from "How to Use t-SNE Effectively".

## Usage

``` r
simplex_data(n, noise = 0.5, color = "#003399")
```

## Arguments

- n:

  Number of points (and hence dimensionality) of the simplex.

- noise:

  Degree by which to perturb the points from their original locations. A
  normal distribution with the specified standard deviation is applied
  to the non-zero coordinate.

- color:

  Color to apply to each point.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xn` columns, and
color in the `color` column.

## Details

Create a dataset of the specified number of points arranged in a rough
simplex, which also determines the dimensionality of the dataset. Each
point has a single non-zero coordinate in a different column to all the
other points.

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
[`subset_clusters_data()`](https://jlmelville.github.io/snedata/reference/subset_clusters_data.md),
[`three_clusters_data()`](https://jlmelville.github.io/snedata/reference/three_clusters_data.md),
[`trefoil_data()`](https://jlmelville.github.io/snedata/reference/trefoil_data.md),
[`two_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_clusters_data.md),
[`two_different_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_different_clusters_data.md),
[`unlink_data()`](https://jlmelville.github.io/snedata/reference/unlink_data.md)

## Examples

``` r
df <- simplex_data(n = 50)
```
