# S-curve with a hole data set.

Simulation data randomly sampled from an S-shaped curve with a hole.

## Usage

``` r
s_curve_hole(n_samples = 100, noise = 0)
```

## Arguments

- n_samples:

  The number of points to create making up the S-shaped curve. Fewer
  than `n_samples` points will be returned because some are removed to
  make the hole.

- noise:

  Add random noise normally-distributed with mean 0 and standard
  deviation `noise`.

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points sampled from an S-shaped curve in 3D, with
optional normally-distributed noise. The S shape is oriented such that
you should be able to see it if you plot the X and Z columns. There is a
circular hole in the middle of the curve, centered at Y = 1.

Points are colored based on their distance along the curve.

This data set is based on
[`s_curve()`](https://jlmelville.github.io/snedata/reference/s_curve.md)
and is used to assess the behavior of the PaCMAP method of Wang and
co-workers (2021).

## References

Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021). Understanding
how dimension reduction tools work: an empirical approach to deciphering
t-SNE, UMAP, TriMAP, and PaCMAP for data visualization. *J Mach. Learn.
Res*, *22*, 1-73.

## See also

the [PaCMAP homepage](https://github.com/YingfanWang/PaCMAP).
