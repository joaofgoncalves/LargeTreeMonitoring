# Print a `ts_breaks` container

Displays a formatted summary of break-detection runs grouped by
algorithm. The output includes detected break indices and dates,
long-term validation, short-term validation, short-term trend
validation, and available diagnostic metrics for each stored run.

## Usage

``` r
# S3 method for class 'ts_breaks'
print(x, digits = 3, max_breaks = 5, ...)
```

## Arguments

- x:

  An object of class `ts_breaks`.

- digits:

  Integer number of decimal places used for numeric values.

- max_breaks:

  Integer maximum number of break indices or dates printed before
  truncating each list.

- ...:

  Additional arguments passed to print methods.

## Value

Invisibly returns `x`.
