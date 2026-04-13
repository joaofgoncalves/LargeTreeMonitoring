# List run identifiers for an algorithm

List run identifiers for an algorithm

## Usage

``` r
ltm_get_runs(ts_breaks_obj, algorithm_name)
```

## Arguments

- ts_breaks_obj:

  An object of class `ts_breaks`.

- algorithm_name:

  Character scalar algorithm name present in `ts_breaks_obj`.

## Value

Character vector of run identifiers for `algorithm_name`.

## See also

Other break result helpers:
[`as.data.frame.ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
[`ltm_add_runs()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
[`ltm_get_algorithms()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_run_details()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md),
[`ltm_summarize_break_df()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md),
[`ltm_ts_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
