# Detect breakpoints using Bayesian change-point models

Fits a two-segment intercept-only model with
[`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html) to a
selected `spidf` time-series column and returns one `ts_breaks_run`
object. The model is fit against a seasonally adjusted series when
`season_adj = TRUE` and the series is long enough for STL decomposition.

## Usage

``` r
ltm_mcp_detect_breaks(
  spidf,
  ts_name = "spi",
  season_adj = TRUE,
  s_window = 30,
  thresh_date,
  sample = "both",
  n_chains = 3,
  n_cores = 3,
  n_adapt = 500,
  n_iter = 1000,
  downsample = NULL,
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

- ts_name:

  Character. Name of the time-series column to analyze. Must be one of
  `VALID_DATA_TYPES`.

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

- sample:

  Character scalar passed to the `sample` argument of
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- n_chains:

  Integer number of MCMC chains passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- n_cores:

  Integer number of cores passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- n_adapt:

  Integer adaptation length passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- n_iter:

  Integer iteration count passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- downsample:

  Optional positive integer. When supplied, the input series is sampled
  every `downsample` rows before model fitting.

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

An object of class `ts_breaks_run` with method `"mcp"`. In addition to
the shared break and validator fields, the object includes MCP-specific
`pars` and `uncertainty_diag` entries when parameter summaries are
available.

## Details

The wrapper uses internally defined priors based on empirical quantiles
and standard deviation of the analysis series. Extra arguments in `...`
are passed to
[`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html) after
resolving deprecated LargeTreeMonitoring aliases `tresh_int` and
`thresh_change`.

Fits a Bayesian one-change-point step model to a preprocessed analysis
series using mcp::mcp(). The analysis series is obtained by optional NA
interpolation followed by optional STL-based seasonal adjustment. The
fitted mcp model is list(ysa ~ 1, ~ 1) with par_x = "di", implying two
constant-mean segments separated by one estimated changepoint. Priors
are data-dependent and directional, favoring a decrease from the first
segment level to the second.

Let \\y_i\\ denote the raw input series at ordered observations \\i = 1,
\ldots, n\\. Missing values are deterministically interpolated before
model fitting; denote the interpolated series by \\\tilde{y}\_i\\. If
seasonal adjustment is enabled and feasible, the fitted series is \\z_i
= SA(\tilde{y}\_i)\\, where \\SA(\cdot)\\ denotes STL-based seasonal
adjustment. Otherwise, \\z_i = \tilde{y}\_i\\.

The MCP specification `list(ysa ~ 1, ~ 1)` with `par_x = "di"` defines a
one-change-point Gaussian step model on \\z_i\\:

\$\$ z_i \mid c, \alpha_1, \alpha_2, \sigma \sim \mathrm{Normal}(\mu_i,
\sigma^2) \$\$

\$\$ \mu_i = \alpha_1 I(i \< c) + \alpha_2 I(i \ge c), \qquad i = 1,
\ldots, n \$\$

where \\c = cp_1\\ is the break index, \\\alpha_1 = int_1\\ is the
pre-break level, and \\\alpha_2 = int_2\\ is the post-break level.

The wrapper uses empirical, data-dependent priors:

\$\$ \alpha_1 \sim \mathrm{Normal}(q\_{0.75}, s^2)\\ \text{truncated
to}\\ \[0, 1\] \$\$

\$\$ \alpha_2 \sim \mathrm{Normal}(q\_{0.25}, s^2)\\ \text{truncated
to}\\ \[0, \alpha_1\] \$\$

\$\$ c \sim \mathrm{Uniform}(1, n) \$\$

where \\q\_{0.75}\\ and \\q\_{0.25}\\ are the empirical 75th and 25th
percentiles of the analysis series and \\s\\ is its empirical standard
deviation.

This prior specification encodes a directed alternative in which the
post-break level cannot exceed the pre-break level. Accordingly, the
model is designed to detect downward level shifts rather than arbitrary
changes in mean level.

## See also

Other break-detection wrappers:
[`ltm_bfast01_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_bfast01_detect_breaks.md),
[`ltm_cpm_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cpm_detect_breaks.md),
[`ltm_strucchange_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_strucchange_detect_breaks.md),
[`ltm_wbs_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_wbs_detect_breaks.md)
