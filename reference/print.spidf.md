# Print method for `spidf` (spectral index dataframe) objects

Displays a summary of a spectral index time series stored as an `spidf`
object, including spatial location, requested and available time ranges,
spectral index type, processing level, coordinate system, and
preprocessing status flags. The underlying data frame is then printed
using the default method.

## Usage

``` r
# S3 method for class 'spidf'
print(x, ...)
```

## Arguments

- x:

  An object of class `spidf`.

- ...:

  Additional arguments passed to the default `print` method.

## Value

Invisibly returns `x`.
