# Plot long-term valid break summaries

Creates a two-panel patchwork plot summarizing long-term valid break
detections. The first panel shows the frequency of valid break dates and
the second panel shows the distribution of break magnitudes by
algorithm.

## Usage

``` r
plot_valid_breaks(df_breaks)
```

## Arguments

- df_breaks:

  Data frame containing at least `algorithm`, `break_date`,
  `break_magn`, and `has_valid_breaks_lt` columns.

## Value

A patchwork object combining two `ggplot` plots.

## See also

Other plotting helpers:
[`ltm_plot_spidf_ts()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_plot_spidf_ts.md)
