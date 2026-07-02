# Visualize NORB object.

Display a NORB object as an image pair.

## Usage

``` r
show_norb_object(
  df,
  category = 0,
  instance = 0,
  elevation = 0,
  azimuth = 0,
  lighting = 0
)
```

## Arguments

- df:

  Data frame containing NORB objects.

- category:

  The category of the object, an integer from 0 to 4.

- instance:

  The instance in the category, an integer from 0 to 9.

- elevation:

  The elevation of the camera, an integer from 0 to 8, representing
  elevations of 30 to 70 degrees from the horizontal, in increments of 5
  degrees.

- azimuth:

  The azimuth, an even integer from 0, 2, 4, ... to 34. Multiply by ten
  to get the value in degrees.

- lighting:

  The lighting condition, represened by an integer in the range 0-5.

## Details

The NORB objects as stored as image pairs and displayed with the first
image in the pair on top, and the second image in the pair below it.

## Examples

``` r
if (FALSE) { # \dontrun{
show_norb_object(norb,
  category = 2, instance = 6, elevation = 6,
  azimuth = 24, lighting = 2
)
# Compare with example at https://github.com/ndrplz/small_norb
} # }
```
