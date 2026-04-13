# Check the stored Google Earth Engine connection status

Reads the `LTM_GEE_STATUS` option set by
[`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md).
This helper does not contact Earth Engine; it reports only the status
known to the current R session.

## Usage

``` r
ltm_check_gee_status()
```

## Value

If Earth Engine was initialized through
[`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md),
returns the stored status string, typically `"CONNECTED"` or
`"NOT_CONNECTED"`. If no status has been recorded, warns and returns
`FALSE`.

## See also

Other Google Earth Engine helpers:
[`ltm_start_gee()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_start_gee.md)
