# Print method for `ts_breaks_run` objects

Displays a formatted summary of a breakpoint-detection run stored as a
`ts_breaks_run` object, including method metadata, detected breaks, and
available long-term, short-term, and short-term trend validation
results. The recorded function call is printed at the end.

## Usage

``` r
# S3 method for class 'ts_breaks_run'
print(x, digits = 4, ...)
```

## Arguments

- x:

  An object of class `ts_breaks_run`.

- digits:

  Integer. Number of decimal places used when printing numeric values.

- ...:

  Additional arguments passed to print methods.

## Value

Invisibly returns `x`.
