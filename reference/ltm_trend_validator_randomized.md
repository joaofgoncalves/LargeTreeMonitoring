# Validate a breakpoint using a randomized short-term trend test

Evaluates whether a detected breakpoint is supported by a short-term
negative trend around the break. The function computes the Theil-Sen
slope on a centered window around `brk` and compares that slope against
a null distribution built from random contiguous pre-break windows of
the same length. Slopes are expressed in percent per year relative to a
robust pre-break baseline. The decision rule can also accept breaks
showing a sustained post-break depression, including partial recovery
cases.

## Usage

``` r
ltm_trend_validator_randomized(
  ysa,
  dts,
  brk,
  trend_window,
  thresh_date,
  trend_require_lower_level = TRUE,
  B = 99,
  seed = NULL,
  post_pct_thresh = -10,
  alpha = 0.1,
  deficit_tol = 0.05,
  min_prop_below = 0.6,
  avg_deficit_thresh = -2.5
)
```

## Arguments

- ysa:

  Numeric vector. Time-series values used for validation.

- dts:

  Date vector or date-like vector of the same length as `ysa`.

- brk:

  Integer. Break index in `ysa`.

- trend_window:

  Integer. Half-window size used on each side of the break. The centered
  validation window excludes the break index itself.

- thresh_date:

  Date or date-like value. The validation window must extend to at least
  this date for the break to be considered valid.

- trend_require_lower_level:

  Logical. If `TRUE`, the mean of the post-break half-window must be
  lower than the mean of the pre-break half-window.

- B:

  Integer. Number of random pre-break windows sampled to build the null
  distribution.

- seed:

  Optional integer seed for reproducible null sampling.

- post_pct_thresh:

  Numeric. Threshold for the centered slope expressed in percent per
  year. Slopes less than or equal to this value support validation.

- alpha:

  Numeric. One-sided p-value threshold used to compare the centered
  slope against the null distribution.

- deficit_tol:

  Numeric. Relative tolerance used to define whether post-break values
  are below the pre-break baseline.

- min_prop_below:

  Numeric. Minimum fraction of post-break observations that must remain
  below the baseline threshold for depression-based validation.

- avg_deficit_thresh:

  Numeric. Maximum allowed mean post-break deficit (in percent relative
  to baseline) for depression-based validation.

## Value

A named list with validation results:

- has_valid_breaks_st_trend:

  Logical indicating whether the break passes the short-term trend
  validator.

- trend_rand_p_value:

  One-sided p-value comparing the centered slope to the randomized
  pre-break null distribution.

- trend_slope_ts:

  Theil-Sen slope for the centered validation window on the original
  scale.

- trend_slope_ts_pct:

  The centered slope expressed as percent per year relative to the
  pre-break baseline.

- trend_rand_null_mean_pct:

  Mean of the null distribution of percent slopes.

- trend_rand_null_sd_pct:

  Standard deviation of the null distribution of percent slopes.

- trend_rand_effect_pct:

  Difference between the centered percent slope and the null mean
  percent slope.

- trend_rand_B:

  Effective number of valid null draws used.

- trend_rand_len:

  Length of the centered validation window.

- post_prop_below_baseline:

  Proportion of post-break observations below the baseline threshold.

- post_avg_deficit_pct:

  Mean post-break percent deficit relative to the pre-break baseline.

## Details

The centered validation window is constructed from `trend_window`
observations before the break and `trend_window` observations after the
break, excluding the break index itself. If the series is too short, if
the centered window is too small, or if there is insufficient pre-break
data to sample equally long null windows, the function returns a result
with `has_valid_breaks_st_trend = FALSE` and missing diagnostics where
appropriate.

A break is considered valid only if the centered window reaches at least
`thresh_date`, any requested lower-level condition is satisfied, and at
least one of the following holds:

1.  the centered percent slope is sufficiently negative;

2.  the centered percent slope is significantly more negative than the
    randomized pre-break null distribution;

3.  the post-break half-window remains sufficiently depressed relative
    to the pre-break baseline.
