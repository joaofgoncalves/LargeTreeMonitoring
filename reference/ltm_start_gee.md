# Initialize Google Earth Engine for LargeTreeMonitoring

Starts a Google Earth Engine session through
[`rgee::ee_Initialize()`](https://r-spatial.github.io/rgee/reference/ee_Initialize.html)
and records the connection status in the `LTM_GEE_STATUS` option.
Functions that retrieve Sentinel-2 data use this option to confirm that
Earth Engine has been initialized before making server-side requests.

## Usage

``` r
ltm_start_gee(user_name = NULL, ...)
```

## Arguments

- user_name:

  Optional character scalar passed to the `user` argument of
  [`rgee::ee_Initialize()`](https://r-spatial.github.io/rgee/reference/ee_Initialize.html).
  Use this when multiple Earth Engine credentials are configured.

- ...:

  Additional arguments passed to
  [`rgee::ee_Initialize()`](https://r-spatial.github.io/rgee/reference/ee_Initialize.html).

## Value

Logical scalar. Returns `TRUE` when initialization succeeds and `FALSE`
when initialization raises an error. As a side effect, sets option
`LTM_GEE_STATUS` to either `"CONNECTED"` or `"NOT_CONNECTED"`.

## See also

Other Google Earth Engine helpers:
[`ltm_check_gee_status()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_gee_status.md)
