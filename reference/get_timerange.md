# Get the time range of a time series

Computes the minimum and maximum dates from the `ti` column of a data
frame or `spidf` object and returns them as a named vector.

## Usage

``` r
get_timerange(x)
```

## Arguments

- x:

  A data frame or `spidf` object containing a `ti` column with date-like
  values.

## Value

A named vector with elements `start_date` and `end_date` of class
`Date`. Returns `NA` values if the input contains no valid dates.
