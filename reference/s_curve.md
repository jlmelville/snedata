# S-curve data set.

Simulation data randomly sampled from an S-shaped curve. Translated from
the [scikit-learn](https://scikit-learn.org/stable/index.html) Python
function `sklearn.datasets.make_s_curve`.

## Usage

``` r
s_curve(n_samples = 100, noise = 0)
```

## Arguments

- n_samples:

  The number of points to create.

- noise:

  Add random noise normally-distributed with mean 0 and standard
  deviation `noise`.

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points sampled from an S-shaped curve in 3D, with
optional normally-distributed noise. The S shape is oriented such that
you should be able to see it if you plot the X and Z columns.

Points are colored based on their distance along the curve.

## References

Buitinck, L., Louppe, G., Blondel, M., Pedregosa, F., Mueller, A.,
Grisel, O., ... & Varoquaux, G. (2013). API design for machine learning
software: experiences from the scikit-learn project. *arXiv preprint*
*arXiv:1309.0238*.
