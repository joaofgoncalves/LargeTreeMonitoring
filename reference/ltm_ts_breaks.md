# Create a break-detection result container

Initializes a `ts_breaks` object for an `spidf` time series. The
container stores the original time series and a nested list of
break-detection runs grouped by algorithm name.

## Usage

``` r
ltm_ts_breaks(spidf_ts)
```

## Arguments

- spidf_ts:

  An object of class `spidf`.

## Value

An object of class `ts_breaks` with elements `spidf_ts` and
`algorithms`.

## See also

Other break result helpers:
[`as.data.frame.ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
[`ltm_add_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
[`ltm_get_algorithms()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md),
[`ltm_get_run_details()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md),
[`ltm_get_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md),
[`ltm_summarize_break_df()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md)
