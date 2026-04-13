# Regularize an `spidf` time series to daily observations

Converts an irregular Sentinel-2 spectral-index time series to a daily
sequence spanning the available data range and imputes missing daily
values with a selected method from imputeTS. The output keeps the
`spidf` metadata from the input and records the regularization status in
attributes.

## Usage

``` r
ltm_regularize_spidf(spidf_obj, method = "linear", use_cloud_mask = TRUE, ...)
```

## Arguments

- spidf_obj:

  An object of class `spidf`.

- method:

  Character scalar interpolation or imputation method. Supported values
  are `"linear"`, `"spline"`, `"stine"`, `"mean"`, `"kalman"`, `"locf"`,
  and `"nocb"`.

- use_cloud_mask:

  Logical scalar. If `TRUE`, interpolate the cloud-masked values in
  `masked_vals`; if `FALSE`, interpolate the raw spectral-index values
  in `spi`.

- ...:

  Additional arguments passed to the selected imputeTS imputation
  function.

## Value

An object of class `spidf` with one row per day, columns `ti`, `spi`,
`original_spi`, `masked_vals`, and `cloud_mask`, and updated attributes
`regularized = TRUE` and `regularize_method = method`.

## See also

Other preprocessing helpers:
[`ltm_apply_moving_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_moving_quantile.md),
[`ltm_apply_whitaker()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_whitaker.md)
