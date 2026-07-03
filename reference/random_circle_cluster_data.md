# Fuzzy 2D Circle

Clusters arranged in a circle from "How to Use t-SNE Effectively".

## Usage

``` r
random_circle_cluster_data(n)
```

## Arguments

- n:

  Number of points to define the circle. These are then jittered as
  described in 'Details', so the total number of observations in the
  data frame will be `20 * n`.

## Value

Data frame with x and y coordinates in the columns `x` and `y`
respectively, and color in the `color` column.

## Details

Creates a dataset where the points are located on the circumference of a
circle of unit radius via
[`circle_data()`](https://jlmelville.github.io/snedata/reference/circle_data.md),
where the angle subtended by the point at the center of the circle has
been uniformly sampled.

Each point is then replicated 20 times, with a slight perturbation to
the original coordinate (using a normal distribution with a standard
deviation of 0.1). Only the twenty jittered points are added to the data
frame. Each point has a color linearly mapped from the angle of the
original unjittered point to a rainbow color scheme.

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
df <- random_circle_data(n = 50)
```
