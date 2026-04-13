# Convert an `spidf` column to a `zoo` time series

Creates a [`zoo::zoo()`](https://rdrr.io/pkg/zoo/man/zoo.html) object
using the selected value column and the `ti` dates stored in an `spidf`
object.

## Usage

``` r
ltm_spidf_to_zoo(spidf_obj, yname)
```

## Arguments

- spidf_obj:

  An object of class `spidf`.

- yname:

  Character scalar naming the column to use as the time-series values.

## Value

A `zoo` object indexed by `spidf_obj$ti`.

## See also

Other time-series conversion helpers:
[`ltm_copy_ts()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_ts.md),
[`ltm_days_between()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_days_between.md),
[`ltm_spidf_to_ts()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_ts.md)
