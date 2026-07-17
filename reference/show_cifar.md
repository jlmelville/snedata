# Visualize CIFAR-10 image.

Display a CIFAR-10 image.

## Usage

``` r
show_cifar(df, n, interpolate = FALSE)
```

## Arguments

- df:

  A legacy CIFAR-10 data frame or canonical image result.

- n:

  Row index of the image to display.

- interpolate:

  If `TRUE`, use linear interpolation to smooth the image. This can help
  when trying to confirm that you really are looking at a tiny image of
  a frog.

## Examples

``` r
if (FALSE) { # \dontrun{
# show the image at position 27001 (it's a plane)
show_cifar(cifar10, 27001)
# bit easier to see it's a plane
show_cifar(cifar10, 27001, interpolate = TRUE)
} # }
```
