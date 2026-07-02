# 2D Curve Dataset.

Simulation data of a 2D polynomial curve.

## Usage

``` r
curve2d()
```

## Value

Data frame with 1450 rows, and 3 columns: `x`, `y` columns contain the
coordinates of the points and `color` the RGB color.

## Details

This data set is used to assess the behavior of the PaCMAP method of
Wang and co-workers (2021) and some related dimensionality reduction
methods, specifically the effect of initialization on what should be an
"easy" dataset to embed.

Points are colored based on their distance along the curve.

## References

Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021). Understanding
how dimension reduction tools work: an empirical approach to deciphering
t-SNE, UMAP, TriMAP, and PaCMAP for data visualization. *J Mach. Learn.
Res*, *22*, 1-73.

## See also

the [PaCMAP homepage](https://github.com/YingfanWang/PaCMAP).
