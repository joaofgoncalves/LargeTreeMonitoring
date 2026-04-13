# Summarize flat break-detection results

Creates a human-readable text summary from a data frame of breakpoint
runs, such as the output of
[`as.data.frame.ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md).
The summary reports the number of runs, detected breaks, validator pass
counts, and grouped summaries for long-term, short-term, and short-term
trend validators when their columns are present.

## Usage

``` r
ltm_summarize_break_df(break_df)
```

## Arguments

- break_df:

  Data frame containing one row per break-detection run. Common columns
  include `method` or `algorithm`, `data_type`, `has_breaks`,
  `break_date`, `break_magn`, `has_valid_breaks_lt`,
  `has_valid_breaks_st`, and `has_valid_breaks_st_trend`.

## Value

Character scalar containing a multi-line summary.

## See also

Other break result helpers:
[`as.data.frame.ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
[`ltm_add_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
[`ltm_get_algorithms()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_run_details()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md),
[`ltm_get_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md),
[`ltm_ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
