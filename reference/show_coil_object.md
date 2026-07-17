# Visualize a COIL object image

Display a COIL-20 or COIL-100 object pose.

## Usage

``` r
show_coil_object(df, object, pose)
```

## Arguments

- df:

  Data frame returned by
  [`download_coil20()`](https://jlmelville.github.io/snedata/reference/download_coil20.md)
  or
  [`download_coil100()`](https://jlmelville.github.io/snedata/reference/download_coil100.md).
  Canonical list results returned with `as = "list"` are also supported.

- object:

  Object id to display. COIL-20 contains objects 1 to 20 and COIL-100
  contains objects 1 to 100.

- pose:

  For COIL-20, the pose id to display, from 0 to 71. For COIL-100, the
  viewing angle, from 0 to 355 in five-degree increments.

## Examples

``` r
if (FALSE) { # \dontrun{
coil20 <- download_coil20()
show_coil_object(coil20, object = 5, pose = 4)
} # }
```
