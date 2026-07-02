# Sphere data set.

Simulation data randomly sampled from the surface of a 3D sphere.

## Usage

``` r
sphere(n = 1000)
```

## Arguments

- n:

  The number of points to create.

## Value

Data frame with `x`, `y`, `z` columns containing the coordinates of the
points and `color` the RGB color.

## Details

Creates a series of points sampled from a 3D spherical surface.

Points are colored based on the angle theta in their spherical
coordinate presentation. You can think of it as dividing the sphere
surface like the segments of an orange.

## References

A dataset like this was used in: Lee, J. A., Peluffo-Ordo'nez, D. H., &
Verleysen, M. (2015). Multi-scale similarities in stochastic neighbour
embedding: Reducing dimensionality while preserving both local and
global structure. *Neurocomputing*, *169*, 246-261.
