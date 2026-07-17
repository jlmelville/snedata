# Toroidal Helix Data Set

Simulation data in the shape of a 3D toroidal helix.

## Usage

``` r
helix(n = 1000, rmajor = 2, rminor = 1, nwinds = 8)
```

## Arguments

- n:

  Number of points to create.

- rmajor:

  Major radius.

- rminor:

  Minor radius.

- nwinds:

  Positive integer number of winds the helix makes.

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points sampled from a 3D helix with the ends joined
to each other.

Unlike
[`ball()`](https://jlmelville.github.io/snedata/reference/ball.md) and
[`sphere()`](https://jlmelville.github.io/snedata/reference/sphere.md),
this data set is not randomly sampled.

Points are colored based on their distances from the origin.

## References

A dataset like this was used in: Lee, J. A., Peluffo-Ordo'nez, D. H., &
Verleysen, M. (2015). Multi-scale similarities in stochastic neighbour
embedding: Reducing dimensionality while preserving both local and
global structure. *Neurocomputing*, *169*, 246-261.

## Examples

``` r
if (FALSE) { # \dontrun{
helix1000 <- helix(n = 1000)
# Coloring should be obvious with a 2D plot of the cross section:
plot(helix1000$x, helix1000$y, col = helix1000$color, pch = 20)
} # }
```
