# Detect breakpoints using change-point models

Applies
[`cpm::detectChangePoint()`](https://rdrr.io/pkg/cpm/man/detectChangePoint.html)
to a selected time-series column in an `spidf` object and returns one
`ts_breaks_run` object. Missing values are interpolated before optional
seasonal adjustment. When a change point is detected, the wrapper
evaluates the same long-term, short-term, and randomized trend
validators used by the other break-detection wrappers.

## Usage

``` r
ltm_cpm_detect_breaks(
  spidf,
  season_adj = TRUE,
  ts_name = "spi",
  s_window = 30,
  cpm_method = "Exponential",
  ARL0 = 500,
  thresh_date,
  lt_window = NULL,
  lt_thresh_change = -10,
  lt_fun = median,
  st_window = NULL,
  st_thresh_change = -10,
  st_fun = median,
  trend_window = NULL,
  trend_require_lower_level = TRUE,
  trend_rand_B = 99,
  trend_rand_seed = 11235,
  trend_post_pct_thresh = -10,
  trend_alpha = 0.1,
  trend_deficit_tol = 0.05,
  trend_min_prop_below = 0.6,
  trend_avg_deficit_thresh = -1.5,
  ...
)
```

## Arguments

- spidf:

  An object of class `spidf`.

- season_adj:

  Logical. If `TRUE`, applies seasonal adjustment before breakpoint
  detection when the series is long enough.

- ts_name:

  Character. Name of the time-series column to analyze. Must be one of
  `VALID_DATA_TYPES`.

- s_window:

  Integer. Seasonal window used by
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) when
  `season_adj = TRUE`.

- cpm_method:

  Character scalar passed to the `cpmType` argument of
  [`cpm::detectChangePoint()`](https://rdrr.io/pkg/cpm/man/detectChangePoint.html).

- ARL0:

  Numeric average run length parameter passed to
  [`cpm::detectChangePoint()`](https://rdrr.io/pkg/cpm/man/detectChangePoint.html).

- thresh_date:

  Date or date-like value. The selected breakpoint must occur on or
  after this date to be considered valid.

- lt_window:

  Optional integer. If provided, long-term validation is computed using
  symmetric windows of this size around the break. Use `NULL` or `0` to
  use all observations before and after the break.

- lt_thresh_change:

  Numeric. Maximum allowed percent change for the long-term validator.
  More negative values indicate stronger declines.

- lt_fun:

  Function. Aggregation function used by the long-term validator,
  typically `median` or `mean`.

- st_window:

  Optional integer. Half-window size for the short-term validator. If
  `NULL`, short-term validation is skipped.

- st_thresh_change:

  Numeric. Maximum allowed percent change for the short-term validator.

- st_fun:

  Function. Aggregation function used for the short-term validator.

- trend_window:

  Optional integer. Half-window size for the randomized short-term trend
  validator. If `NULL`, trend validation is skipped.

- trend_require_lower_level:

  Logical. If `TRUE`, the post-break mean must be lower than the
  pre-break mean in the trend validator.

- trend_rand_B:

  Integer. Number of randomized pre-break windows used to build the null
  distribution in the trend validator.

- trend_rand_seed:

  Optional integer seed for reproducible randomized trend validation.

- trend_post_pct_thresh:

  Numeric. Percent-slope threshold used by the trend validator.

- trend_alpha:

  Numeric. One-sided p-value threshold used by the trend validator.

- trend_deficit_tol:

  Numeric. Relative tolerance used to define below-baseline post-break
  observations in the trend validator.

- trend_min_prop_below:

  Numeric. Minimum proportion of post-break values that must remain
  below baseline for depression-based trend validation.

- trend_avg_deficit_thresh:

  Numeric. Maximum allowed mean post-break deficit, in percent, for
  depression-based trend validation.

- ...:

  Additional arguments reserved for future use.

## Value

An object of class `ts_breaks_run` with method `"cpm"`. The object
records the detected change point, its date, validation flags and
diagnostics, the raw
[`cpm::detectChangePoint()`](https://rdrr.io/pkg/cpm/man/detectChangePoint.html)
result in `output_object`, and the matched call.

## Details

The CPM startup period is computed as the number of days between the
available series start date and `thresh_date`. Extra arguments in `...`
are passed to
[`cpm::detectChangePoint()`](https://rdrr.io/pkg/cpm/man/detectChangePoint.html)
after resolving deprecated LargeTreeMonitoring aliases `tresh_int` and
`thresh_change`.

## See also

Other break-detection wrappers:
[`ltm_bfast01_detect_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_bfast01_detect_breaks.md),
[`ltm_mcp_detect_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_mcp_detect_breaks.md),
[`ltm_strucchange_detect_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_strucchange_detect_breaks.md),
[`ltm_wbs_detect_breaks()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_wbs_detect_breaks.md)
