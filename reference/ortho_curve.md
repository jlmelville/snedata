# Mutually Orthogonal Step Data

Points related by mutually orthogonal steps, from "How to Use t-SNE
Effectively".

## Usage

``` r
ortho_curve(n)
```

## Arguments

- n:

  Number of points to sample.

## Value

Data frame with coordinates in the `X1`, `X2` ... `Xn` columns, and
color in the `color` column.

## Details

Creates N points from an N-dimensional space, where each point is
related by a mutually orthogonal step. Coordinates from a triangular
matrix, where the coordinates in the triangle are one, and are zero
everywhere else. Points are linearly mapped from their row id to a
rainbow color scheme.

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
df <- ortho_curve(n = 50)
```
