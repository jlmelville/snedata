# Swiss Roll Data Set

Simulation data randomly sampled from a 2D plane curled up into 3
dimensions.

## Usage

``` r
swiss_roll(n = 1000, min_phi = 1.5 * pi, max_phi = 4.5 * pi, max_z = 10)
```

## Arguments

- n:

  Number of points to create.

- min_phi:

  Minimum value of the range of \\phi\\ to sample from

- max_phi:

  Maximum value of the range of \\phi\\ to sample from.

- max_z:

  Maximum value of the `z` range to sample from (minimum values is
  always 0).

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points randomly sampled from a swiss roll-shaped
manifold: a two-dimensional plane which has been rolled up into a spiral
shape. Or just look at a swiss roll.

The formula for sampling the x, y and z coordinates used in this dataset
is from that given in the Stochastic Proximity Embedding paper by
Agrafiotis and Xu (I don't know who originally came up with the data
set, though): \$\$x = \phi cos\phi, y = \phi sin \phi, z\$\$

where \\\phi\\ is sampled between \\\frac{3\pi}{2}\\ and
\\\frac{5\pi}{2}\\, and `z` is sampled between 0 and 10 (the range of
\\\phi\\ and `z` can be modified, if desired).

Points are colored based on the value of phi. If you unrolled the
manifold into a flat sheet, you would see the color change linearly in
the direction you unrolled it. Or just plot the x-y cross section (see
the examples).

## References

I first saw the equations in

Agrafiotis, D. K., & Xu, H. (2002). A self-organizing principle for
learning nonlinear manifolds. *Proceedings of the National Academy of
Sciences*, *99*(25), 15869-15872.

But the dataset turns up everywhere, most notably:

Roweis, S. T., & Saul, L. K. (2000). Nonlinear dimensionality reduction
by locally linear embedding. *Science*, *290*(5500), 2323-2326.

If the idea of flattening a Swiss Roll didn't originate in that
publication, it was certainly popularized for its use in nonlinear
dimensionality reduction. A Matlab-formatted version of that dataset is
still available at:

[http://web.mit.edu/cocosci/isomap/datasets.html](http://web.mit.edu/cocosci/isomap/datasets.md)

I'm not sure exactly what parameters were used to generate it, but you
can get something similar by calling:
`swiss_roll(n = 20000, min_phi = 1.5 * pi, max_phi = 4.5 * pi, max_z = 50)`

## Examples

``` r
if (FALSE) { # \dontrun{
swiss1000 <- swiss_roll(n = 1000)
# Coloring should be obvious with a 2D plot of the cross section:
plot(swiss1000$x, swiss1000$y, col = swiss1000$color, pch = 20)
} # }
```
