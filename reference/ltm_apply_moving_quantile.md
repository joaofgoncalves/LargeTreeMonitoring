# Apply a moving quantile to a regularized `spidf` time series

Computes a centered moving quantile over the daily `spi` column of a
regularized `spidf` object. The result is stored in `spi_mov_wind`;
existing metadata are preserved and moving-window attributes are
updated.

## Usage

``` r
ltm_apply_moving_quantile(spidf_obj, quant = 0.95, win_size = 9)
```

## Arguments

- spidf_obj:

  A regularized object of class `spidf`, typically returned by
  [`ltm_regularize_spidf()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md).

- quant:

  Numeric scalar probability passed to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- win_size:

  Integer window size in days. The window is split around each
  observation using `floor(win_size / 2)` days before and
  `ceiling(win_size / 2)` days after.

## Value

An object of class `spidf` with an added `spi_mov_wind` column and
updated attributes `mov_window`, `mov_window_quantile`, and
`mov_window_size_days`.

## See also

Other preprocessing helpers:
[`ltm_apply_whitaker()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_apply_whitaker.md),
[`ltm_regularize_spidf()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_regularize_spidf.md)
