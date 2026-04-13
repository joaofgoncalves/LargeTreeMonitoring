# Convert an `spidf` column to a regular `ts` object

Creates a base R [`stats::ts()`](https://rdrr.io/r/stats/ts.html) object
from an `spidf` value column. Leap-day rows are removed before
conversion so the default annual frequency of 365 remains aligned with
day-of-year indexing.

## Usage

``` r
ltm_spidf_to_ts(spidf_obj, yname, freq = 365)
```

## Arguments

- spidf_obj:

  An object of class `spidf`.

- yname:

  Character scalar naming the column to use as the time-series values.

- freq:

  Numeric frequency passed to
  [`stats::ts()`](https://rdrr.io/r/stats/ts.html). The default is
  `365`.

## Value

A `ts` object whose start is derived from the first non-leap date in
`spidf_obj$ti`.

## See also

Other time-series conversion helpers:
[`ltm_copy_ts()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_ts.md),
[`ltm_days_between()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_days_between.md),
[`ltm_spidf_to_zoo()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_zoo.md)
