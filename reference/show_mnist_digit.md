# Visualize MNIST digit.

Display the requested row of an MNIST-family dataset result.

## Usage

``` r
show_mnist_digit(df, n, col = grDevices::gray(1:255/255), ...)
```

## Arguments

- df:

  A legacy MNIST-family data frame or canonical image result.

- n:

  Row index of the digit to display.

- col:

  List of colors to use in the display.

- ...:

  Other arguments passed onto the
  [`graphics::image()`](https://rdrr.io/r/graphics/image.html) function.

## Note

Originally based on a function by Brendan O'Connor, which can be found
at <https://gist.github.com/brendano/39760>.

## Examples

``` r
if (FALSE) { # \dontrun{
# show the fifth digit
mnist <- download_mnist()
show_mnist_digit(mnist, 5)
} # }
```
