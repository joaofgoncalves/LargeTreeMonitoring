# Package index

## All functions

- [`PROC_LEVELS_LIST`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/PROC_LEVELS_LIST.md)
  : Supported Sentinel-2 processing levels

- [`SPECTRAL_INDICES_LIST`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/SPECTRAL_INDICES_LIST.md)
  : Supported Sentinel-2 spectral indices

- [`VALID_DATA_TYPES`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/VALID_DATA_TYPES.md)
  : Supported time-series columns for break detection

- [`as.data.frame(`*`<ts_breaks>`*`)`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md)
  :

  Convert a `ts_breaks` container to a data frame

- [`ltm_add_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_add_runs.md)
  :

  Add break-detection runs to a `ts_breaks` container

- [`ltm_app()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_app.md)
  : Create the LargeTreeMonitoring Shiny app

- [`ltm_apply_moving_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_moving_quantile.md)
  :

  Apply a moving quantile to a regularized `spidf` time series

- [`ltm_apply_whitaker()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_whitaker.md)
  :

  Apply Whittaker smoothing to a regularized `spidf` time series

- [`ltm_bfast01_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_bfast01_detect_breaks.md)
  : Detect breakpoints using BFAST01

- [`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md)
  : Get the cache directory

- [`ltm_cache_file_name()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_file_name.md)
  : Build the cache file name for a Sentinel-2 request

- [`ltm_check_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_cache.md)
  : Look up a cached Sentinel-2 request

- [`ltm_check_gee_status()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_gee_status.md)
  : Check the stored Google Earth Engine connection status

- [`ltm_config_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md)
  : Get the user configuration directory

- [`ltm_copy_metadata()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_metadata.md)
  :

  Copy `spidf` metadata attributes

- [`ltm_copy_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_ts.md)
  : Copy time-series attributes to new values

- [`ltm_cpm_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cpm_detect_breaks.md)
  : Detect breakpoints using change-point models

- [`ltm_days_between()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_days_between.md)
  : Count days between two dates

- [`ltm_ed_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ed_detect_breaks.md)
  : Detect breakpoints using energy divisive segmentation

- [`ltm_ee_feature_collection_to_df()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ee_feature_collection_to_df.md)
  : Convert an Earth Engine FeatureCollection to a data frame

- [`ltm_ee_features_to_df()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ee_features_to_df.md)
  : Convert a list of Earth Engine features to a data frame

- [`ltm_get_algorithms()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_algorithms.md)
  :

  List algorithms stored in a `ts_breaks` container

- [`ltm_get_run_details()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_run_details.md)
  : Retrieve one break-detection run

- [`ltm_get_runs()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_get_runs.md)
  : List run identifiers for an algorithm

- [`ltm_mcp_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_mcp_detect_breaks.md)
  : Detect breakpoints using Bayesian change-point models

- [`ltm_plot_spidf_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_plot_spidf_ts.md)
  :

  Plot an `spidf` time series and detected breaks

- [`ltm_regularize_spidf()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md)
  :

  Regularize an `spidf` time series to daily observations

- [`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md)
  : Extract a Sentinel-2 spectral index time series for a point or
  buffer

- [`ltm_save_to_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_save_to_cache.md)
  :

  Save an `spidf` object to the package cache

- [`ltm_spidf_to_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_ts.md)
  :

  Convert an `spidf` column to a regular `ts` object

- [`ltm_spidf_to_zoo()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_zoo.md)
  :

  Convert an `spidf` column to a `zoo` time series

- [`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md)
  : Initialize Google Earth Engine for LargeTreeMonitoring

- [`ltm_strucchange_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_strucchange_detect_breaks.md)
  : Detect breakpoints using structural-change segmentation

- [`ltm_summarize_break_df()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_summarize_break_df.md)
  : Summarize flat break-detection results

- [`ltm_trend_validator_randomized()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_trend_validator_randomized.md)
  : Validate a breakpoint using a randomized short-term trend test

- [`ltm_ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_ts_breaks.md)
  : Create a break-detection result container

- [`ltm_wbs_detect_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_wbs_detect_breaks.md)
  : Detect breakpoints using wild binary segmentation

- [`plot_valid_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/plot_valid_breaks.md)
  : Plot long-term valid break summaries

- [`print(`*`<spidf>`*`)`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/print.spidf.md)
  :

  Print method for `spidf` (spectral index dataframe) objects

- [`print(`*`<ts_breaks>`*`)`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/print.ts_breaks.md)
  :

  Print a `ts_breaks` container

- [`print(`*`<ts_breaks_run>`*`)`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/print.ts_breaks_run.md)
  :

  Print method for `ts_breaks_run` objects

- [`run_ltm_app()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/run_ltm_app.md)
  : Run the LargeTreeMonitoring Shiny app

- [`get_latitude()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_longitude()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_start_date()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_end_date()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_spi()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_proc_level()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_crs_code()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`is_regularized()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_regularize_method()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`has_mov_window()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`is_whit_smoothed()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_mov_window_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_mov_window_size_days()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_whit_lambda()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_whit_quantile_threshold()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_whit_weights_used()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_range_start()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_range_end()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_tree_id()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_use_buffer()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_buffer_radius_m()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  [`get_cloud_mask_threshold()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md)
  :

  Get metadata from an `spidf` object

- [`set_latitude()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_longitude()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_start_date()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_end_date()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_spi()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_proc_level()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_crs_code()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_regularized()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_regularize_method()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_mov_window()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_whit_smoothed()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_mov_window_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_mov_window_size_days()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_whit_lambda()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_whit_quantile_threshold()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_whit_weights_used()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_range_start()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_range_end()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_tree_id()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_use_buffer()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_buffer_radius_m()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  [`set_cloud_mask_threshold()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
  :

  Set metadata on an `spidf` object
