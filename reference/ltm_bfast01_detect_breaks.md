# Detect breakpoints using BFAST01

Applies [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html)
to a selected `spidf` time-series column and returns one `ts_breaks_run`
object. Detection is performed by BFAST01 on the converted `ts` series.
Break magnitude and validation diagnostics are computed from fitted
values, with seasonal adjustment applied to those fitted values when the
series is long enough for STL decomposition.

## Usage

``` r
ltm_bfast01_detect_breaks(
  spidf,
  ts_name = "spi",
  formula = response ~ harmon + trend,
  s_window = 30,
  test = "OLS-MOSUM",
  level = 0.05,
  aggregate = all,
  trim = NULL,
  bandwidth = 0.15,
  functional = "max",
  order = 3,
  thresh_date,
  lt_window = NULL,
  lt_thresh_change = -10,
  lt_fun = median,
  st_window = NULL,
  st_thresh_change = -5,
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

- ts_name:

  Character. Name of the time-series column to analyze. Must be one of
  `VALID_DATA_TYPES`.

- formula:

  Formula passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html). The
  default models the response with harmonic seasonal terms and trend.

- s_window:

  Integer. Seasonal window used by
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) when
  `season_adj = TRUE`.

- test:

  Character scalar test name passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

- level:

  Numeric significance level passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

- aggregate:

  Function passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html) for
  aggregating test results.

- trim:

  Optional trimming parameter passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

- bandwidth:

  Numeric bandwidth passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

- functional:

  Character scalar functional passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

- order:

  Integer harmonic order passed to
  [`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html).

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

An object of class `ts_breaks_run` with method `"bfast01"`. The object
records the selected break, validation flags and diagnostics, the raw
[`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html) result
in `output_object`, and the matched call.

## Details

Extra arguments in `...` are passed to
[`bfast::bfast01()`](https://rdrr.io/pkg/bfast/man/bfast01.html) after
resolving deprecated LargeTreeMonitoring aliases `tresh_int` and
`thresh_change`. Although this wrapper has an `s_window` argument for
validation-time seasonal adjustment of fitted values, it does not expose
a `season_adj` argument and records `season_adj = FALSE` in its return
object.

## See also

Other break-detection wrappers:
[`ltm_cpm_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cpm_detect_breaks.md),
[`ltm_mcp_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_mcp_detect_breaks.md),
[`ltm_strucchange_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_strucchange_detect_breaks.md),
[`ltm_wbs_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_wbs_detect_breaks.md)
