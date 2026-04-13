# LargeTreeMonitoring package

LargeTreeMonitoring supports tree-level monitoring from Sentinel-2 time
series extracted with Google Earth Engine. It provides tools to retrieve
spectral-index observations for individual tree locations, preprocess
the resulting time series, run multiple breakpoint-detection algorithms,
validate detected breaks, summarize results, and launch an interactive
Shiny workflow. Although the package provides general tools for
time-series analysis and breakpoint detection, its current primary focus
is the detection of large tree loss or removal.

## Details

The package is organized around the `spidf` object, a data-frame
subclass that stores spectral-index observations and request metadata. A
typical analysis starts by initializing Google Earth Engine with
[`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md),
retrieving a Sentinel-2 time series with
[`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md),
applying optional preprocessing, running one or more
breakpoint-detection wrappers, and collecting the results in a
[`ltm_ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
container.

## Main workflow

1.  Initialize Google Earth Engine with
    [`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md)
    and verify the stored session status with
    [`ltm_check_gee_status()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_gee_status.md).

2.  Retrieve an `spidf` time series with
    [`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md).
    Supported spectral indices are listed in
    [`SPECTRAL_INDICES_LIST`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/SPECTRAL_INDICES_LIST.md),
    and supported Sentinel-2 processing levels are listed in
    [`PROC_LEVELS_LIST`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/PROC_LEVELS_LIST.md).

3.  Optionally regularize and smooth the time series with
    [`ltm_regularize_spidf()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md),
    [`ltm_apply_moving_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_moving_quantile.md),
    and
    [`ltm_apply_whitaker()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_whitaker.md).

4.  Run breakpoint-detection wrappers such as:

    - [`ltm_ed_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ed_detect_breaks.md)
      – Energy-divisive
      ([ecp::e.divisive](https://rdrr.io/pkg/ecp/man/e.divisive.html)),

    - [`ltm_cpm_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cpm_detect_breaks.md)
      – Change Point Models (cpm::cpm-package),

    - [`ltm_bfast01_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_bfast01_detect_breaks.md)
      – BFAST
      ([bfast::bfast-package](https://rdrr.io/pkg/bfast/man/bfast-package.html)),

    - [`ltm_mcp_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_mcp_detect_breaks.md)
      – our model based on a specific implementation ob a Bayesian
      Regression with Multiple Change Points
      ([mcp::mcp-package](https://lindeloev.github.io/mcp/reference/mcp-package.html)),

    - [`ltm_strucchange_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_strucchange_detect_breaks.md)
      – Structural Change in Linear Regression Models
      ([strucchangeRcpp::breakpoints](https://rdrr.io/pkg/strucchangeRcpp/man/breakpoints.html)
      ), and

    - [`ltm_wbs_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_wbs_detect_breaks.md)
      – Wild Binary Segmentation for multiple change-point detection
      ([wbs::wbs-package](https://rdrr.io/pkg/wbs/man/wbs-package.html)).

5.  Store, inspect, flatten, plot, and summarize results with
    [`ltm_ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md),
    [`ltm_add_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md),
    [`as.data.frame.ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md),
    [`ltm_plot_spidf_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_plot_spidf_ts.md),
    [`plot_valid_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/plot_valid_breaks.md),
    and
    [`ltm_summarize_break_df()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md).

## Data model

`spidf` objects contain at least `id`, `ti`, `masked_vals`, `spi`, and
`cloud_mask` columns. Metadata such as coordinates, date ranges, tree
identifiers, buffer settings, spectral index, processing level, and
preprocessing status are stored as attributes and can be read or updated
with the grouped `spidf` accessor and setter functions documented in
[`spidf_accessors`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
and
[`spidf_setters`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md).

## Break detection and validation

Break-detection wrappers return `ts_breaks_run` objects. Each wrapper
records the method name, analyzed data column, detected break index and
date when available, the raw upstream algorithm result,
seasonal-adjustment metadata, and validation diagnostics. Long-term and
short-term validators compare pre-break and post-break summaries, while
the randomized trend validator implemented by
[`ltm_trend_validator_randomized()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_trend_validator_randomized.md)
evaluates short-term negative trend evidence around a candidate break.

## Cache and configuration

Cache files are stored under
[`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md).
The cache location can be configured with option
`LargeTreeMonitoring.cache_dir` or environment variable `LTM_CACHE_DIR`;
otherwise the package uses the platform-specific R user cache directory
and falls back to the legacy `ltm_cache` directory when needed. User app
configuration is stored under
[`ltm_config_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md).

## Package options

- `LargeTreeMonitoring.cache_dir`:

  Optional character scalar path used by
  [`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md)
  as the first candidate cache directory.

## Interactive app

[`ltm_app()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_app.md)
constructs the packaged Shiny application, and
[`run_ltm_app()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/run_ltm_app.md)
launches it. The app exposes the core workflow for selecting locations,
retrieving Sentinel-2 data, preprocessing time series, running
breakpoint detection, and downloading tabular results.

## See also

[`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md),
[`ltm_regularize_spidf()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md),
[`ltm_ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md),
[`ltm_app()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_app.md)

## Author

**Maintainer**: João Gonçalves <joao.goncalves@cibio.up.pt>
([ORCID](https://orcid.org/0000-0002-6615-0218))

Other contributors:

- João Soutinho <soutinhojg@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-1036-2625)) \[contributor, data
  contributor, reviewer\]
