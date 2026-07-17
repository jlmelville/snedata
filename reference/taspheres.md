# High Dimensional Spheres Dataset

Creates a dataframe consisting of samples from the d-spheres of radius
`r` enclosed within a larger d-sphere of radius `5 * r`.

## Usage

``` r
taspheres(n_samples = 500, d = 100, n_spheres = 11, r = 5)
```

## Arguments

- n_samples:

  Number of points to sample from each of the `n_spheres` d-spheres. The
  larger d-sphere has `10 * n_samples` points.

- d:

  The dimensionality of each sphere. The returned dataframe will have
  the `d + 1` dimensions of the Euclidean space in which the sphere is
  embedded.

- n_spheres:

  Number of spheres to return. There will be `n_spheres - 1` small
  spheres and 1 larger sphere.

- r:

  The radius of each of the smaller spheres. The larger sphere has
  radius `5 * r`.

## Value

Data frame with `d + 1` numerical columns containing the coordinates of
the d-spheres and a `labels` factor column giving the identity of each
d-sphere: levels `0 .. n_spheres - 2` are the smaller d-spheres. Level
`n_spheres - 1` is the label for the big d-sphere.

## Details

This dataset was used by Moor and co-workers in their "Topological
Autoencoders" paper and this function is based on the Python code in the
GitHub repo for the paper.

## References

Moor, M., Horn, M., Rieck, B., & Borgwardt, K. (2020). Topological
Autoencoders. In *Proceedings of the 37th International Conference on
Machine Learning (ICML)* (pp. 7045–7054). PMLR.

<https://michaelmoor.ml/blog/topoae/main/>

<https://github.com/BorgwardtLab/topological-autoencoders>
