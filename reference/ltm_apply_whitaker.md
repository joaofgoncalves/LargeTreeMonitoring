# Apply Whittaker smoothing to a regularized `spidf` time series

Smooths the daily `spi` series with
[`phenofit::whit2()`](https://rdrr.io/pkg/phenofit/man/whit2.html). When
moving-window values are present, the function also smooths
`spi_mov_wind` into `spi_mov_smooth`. Optional weights can reduce the
influence of lower spectral-index values before smoothing the main `spi`
series.

## Usage

``` r
ltm_apply_whitaker(
  spidf_obj,
  lambda = 5000,
  quantile_thresh = 0.35,
  use_weights = TRUE,
  min_weight = 0.01
)
```

## Arguments

- spidf_obj:

  A regularized object of class `spidf`, typically returned by
  [`ltm_regularize_spidf()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md).

- lambda:

  Numeric smoothing parameter passed to
  [`phenofit::whit2()`](https://rdrr.io/pkg/phenofit/man/whit2.html).

- quantile_thresh:

  Numeric probability used to compute the weighting threshold when
  `use_weights = TRUE`.

- use_weights:

  Logical scalar. If `TRUE`, values greater than or equal to the
  `quantile_thresh` quantile receive weight `1`, lower non-missing
  values receive `min_weight`, and missing values receive `0`.

- min_weight:

  Numeric weight assigned to finite values below the threshold when
  `use_weights = TRUE`.

## Value

An object of class `spidf` with `spi_smooth`, optional `spi_weight`, and
optional `spi_mov_smooth` columns, plus updated Whittaker-smoothing
attributes.

## See also

Other preprocessing helpers:
[`ltm_apply_moving_quantile()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_moving_quantile.md),
[`ltm_regularize_spidf()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md)
