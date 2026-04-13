# Convert a `ts_breaks` container to a data frame

Flattens a `ts_breaks` container into one row per stored run. Each row
contains algorithm metadata, the primary detected break when present,
long-term validation fields, short-term validation fields, and
randomized trend-validation diagnostics.

## Usage

``` r
# S3 method for class 'ts_breaks'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An object of class `ts_breaks`.

- row.names:

  Ignored; included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Ignored; included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Additional arguments ignored by this method.

## Value

A data frame with a stable schema containing one row per stored run.
Empty containers return a zero-row data frame with the same columns.

## See also

Other break result helpers:
[`ltm_add_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
[`ltm_get_algorithms()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_run_details()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md),
[`ltm_get_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md),
[`ltm_summarize_break_df()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md),
[`ltm_ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
