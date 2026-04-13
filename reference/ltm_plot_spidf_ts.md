# Plot an `spidf` time series and detected breaks

Builds a `ggplot` visualization of available spectral-index series
stored in an `spidf` object. The plot can overlay break dates from a
flat break-detection data frame and optionally restrict them to
validator-approved breaks.

## Usage

``` r
ltm_plot_spidf_ts(
  spidf_obj,
  tree_id = "",
  df_breaks = NULL,
  only_valid_breaks = TRUE,
  valid_breaks_mode = "any",
  validators_sel = NULL
)
```

## Arguments

- spidf_obj:

  An object of class `spidf`.

- tree_id:

  Optional tree identifier shown in the plot subtitle when not empty and
  not `"---"`.

- df_breaks:

  Optional data frame of break-detection results, typically produced by
  [`as.data.frame.ts_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/as.data.frame.ts_breaks.md).

- only_valid_breaks:

  Logical scalar. If `TRUE`, plot only breaks that pass at least one
  selected validator or all selected validators according to
  `valid_breaks_mode`.

- valid_breaks_mode:

  Character scalar. Either `"any"` to retain breaks passing any selected
  validator or `"all"` to retain breaks passing all selected validators.

- validators_sel:

  Optional character vector selecting validator groups used when
  `only_valid_breaks = TRUE`. Supported values are `"lt"`, `"st"`, and
  `"st_trend"`. When `NULL`, all available validator columns are
  considered.

## Value

A `ggplot` object.

## See also

Other plotting helpers:
[`plot_valid_breaks()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/plot_valid_breaks.md)
