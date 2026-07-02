# Ball Data Set

Simulation data randomly sampled from the entire volume of a ball of the
specified dimension (by default, 3-dimensional).

## Usage

``` r
ball(n = 1000, rad = 1, ndim = 3)
```

## Arguments

- n:

  Number of points to create.

- rad:

  Radius of the ball.

- ndim:

  Dimension of the ball.

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points sampled from an `ndim`-dimensional spherical
volume. Points are colored based on the square of their distance from
the origin.

## References

A dataset like this was used in: Lee, J. A., Peluffo-Ordo'nez, D. H., &
Verleysen, M. (2015). Multi-scale similarities in stochastic neighbour
embedding: Reducing dimensionality while preserving both local and
global structure. *Neurocomputing*, *169*, 246-261.
