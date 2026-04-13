# Copy time-series attributes to new values

Builds a new [`stats::ts()`](https://rdrr.io/r/stats/ts.html) object
with the same start, end, and frequency as an existing `ts` object,
replacing only the observed values.

## Usage

``` r
ltm_copy_ts(ts_to_copy, values)
```

## Arguments

- ts_to_copy:

  A base R `ts` object whose time attributes are copied.

- values:

  Vector of replacement values. Its length must equal
  `length(ts_to_copy)`.

## Value

A `ts` object containing `values` and the time attributes of
`ts_to_copy`.

## See also

Other time-series conversion helpers:
[`ltm_days_between()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_days_between.md),
[`ltm_spidf_to_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_ts.md),
[`ltm_spidf_to_zoo()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_zoo.md)
