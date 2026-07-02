# Visualize Olivetti face.

Display an image from the Olivetti faces dataset.

## Usage

``` r
show_olivetti_face(df, face, pose, col = grDevices::gray(1/12:1))
```

## Arguments

- df:

  Data frame containing the Olivetti faces.

- face:

  Face index of the image to display. Must be an integer between 1 and
  40.

- pose:

  Pose index of the image to display. Must be an integer between 1 and
  10.

- col:

  List of colors to use in the display.
