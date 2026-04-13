# Retrieve one break-detection run

Retrieve one break-detection run

## Usage

``` r
ltm_get_run_details(ts_breaks_obj, algorithm_name, run_id)
```

## Arguments

- ts_breaks_obj:

  An object of class `ts_breaks`.

- algorithm_name:

  Character scalar algorithm name present in `ts_breaks_obj`.

- run_id:

  Character scalar run identifier for `algorithm_name`.

## Value

A list representing one stored `ts_breaks_run`.

## See also

Other break result helpers:
[`as.data.frame.ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
[`ltm_add_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
[`ltm_get_algorithms()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md),
[`ltm_summarize_break_df()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md),
[`ltm_ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
