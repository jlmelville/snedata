# Visualize Isomap face.

Display an image from the Isomap face-pose dataset.

## Usage

``` r
show_isomap_face(df, n, col = grDevices::gray(1:255/255), ...)
```

## Arguments

- df:

  Data frame containing Isomap faces, as returned by
  [`download_isomap_faces`](https://jlmelville.github.io/snedata/reference/download_isomap_faces.md).

- n:

  Row index of the image to display.

- col:

  List of colors to use in the display.

- ...:

  Other arguments passed onto the
  [`image`](https://rdrr.io/r/graphics/image.html) function.
