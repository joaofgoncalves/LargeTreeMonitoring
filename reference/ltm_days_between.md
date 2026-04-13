# Count days between two dates

Coerces both inputs to `Date` and returns the integer day difference
`end_date - start_date`.

## Usage

``` r
ltm_days_between(start_date, end_date)
```

## Arguments

- start_date:

  Start date or value coercible to `Date`.

- end_date:

  End date or value coercible to `Date`.

## Value

Integer scalar number of days between `start_date` and `end_date`.

## See also

Other time-series conversion helpers:
[`ltm_copy_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_ts.md),
[`ltm_spidf_to_ts()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_ts.md),
[`ltm_spidf_to_zoo()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_spidf_to_zoo.md)
