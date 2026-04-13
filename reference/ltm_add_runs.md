# Add break-detection runs to a `ts_breaks` container

Adds one or more `ts_breaks_run` objects to a `ts_breaks` container.
Runs are grouped by their `method` field and assigned sequential run
identifiers such as `"run-01"`.

## Usage

``` r
ltm_add_runs(ts_breaks_obj, ...)
```

## Arguments

- ts_breaks_obj:

  An object of class `ts_breaks`.

- ...:

  One or more objects of class `ts_breaks_run`, typically returned by
  the break-detection wrapper functions.

## Value

The updated `ts_breaks` object.

## See also

Other break result helpers:
[`as.data.frame.ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
[`ltm_get_algorithms()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_run_details()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md),
[`ltm_get_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md),
[`ltm_summarize_break_df()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md),
[`ltm_ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
