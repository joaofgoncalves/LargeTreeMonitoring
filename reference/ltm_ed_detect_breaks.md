# Detect breakpoints using energy divisive segmentation

Applies the energy divisive algorithm from ecp to a time series stored
in an `spidf` object and returns a single selected breakpoint together
with long-term, short-term, and optional randomized trend-based
validation diagnostics.

## Usage

``` r
ltm_ed_detect_breaks(
  spidf,
  ts_name = "spi",
  sig_lvl = 0.05,
  R = 1000,
  k = 1,
  min_size = 30,
  alpha = 1,
  season_adj = TRUE,
  s_window = 30,
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
  trend_alpha = 0.05,
  trend_deficit_tol = 0.05,
  trend_min_prop_below = 0.6,
  trend_avg_deficit_thresh = -1.5,
  break_select = c("first", "first_after_date", "largest_drop"),
  ...
)
```

## Arguments

- spidf:

  An object of class `spidf`.

- ts_name:

  Character. Name of the time-series column to analyze. Must be one of
  `VALID_DATA_TYPES`.

- sig_lvl:

  Numeric. Significance level passed to
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).

- R:

  Integer. Number of permutations passed to
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).

- k:

  Integer. Additional argument passed to
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).

- min_size:

  Integer. Minimum cluster size passed to
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).

- alpha:

  Numeric. Significance level adjustment parameter passed to
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).

- season_adj:

  Logical. If `TRUE`, applies seasonal adjustment before breakpoint
  detection when the series is long enough.

- s_window:

  Integer. Seasonal window used by
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) when
  `season_adj = TRUE`.

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

- break_select:

  Character. Rule used to select a breakpoint when multiple candidates
  are detected. One of `"first"`, `"first_after_date"`, or
  `"largest_drop"`.

- ...:

  Additional arguments reserved for future use.

## Value

An object of class `ts_breaks_run`. It contains the selected breakpoint,
the percent magnitude of the long-term change, flags for detected and
validated breaks, the raw
[`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html)
output, preprocessing metadata, and diagnostics from the short-term and
randomized trend validators.

## Details

The selected series can be seasonally adjusted before breakpoint
detection. When multiple candidate breakpoints are detected, one
breakpoint is selected according to `break_select`.

If the input series is too short, contains too few observations after
preprocessing, or no valid breakpoint candidates are found, the function
returns a `ts_breaks_run` object with `has_breaks = FALSE` and
validation fields set to `FALSE` or `NA` as appropriate.

Missing values are interpolated before seasonal adjustment. Seasonal
adjustment is only applied when `season_adj = TRUE` and the series is
long enough for [`stats::stl()`](https://rdrr.io/r/stats/stl.html).
Otherwise, the original series is used.

The long-term validator compares aggregated values before and after the
selected break. The short-term validator compares local pre- and
post-break windows when `st_window` is provided. The randomized trend
validator calls
[`ltm_trend_validator_randomized()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_trend_validator_randomized.md)
when `trend_window` is provided.
