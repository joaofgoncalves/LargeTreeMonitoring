# Build the cache file name for a Sentinel-2 request

Constructs the canonical RDS cache path for a Sentinel-2 time-series
request. Coordinates are rounded to five decimal places, dates are
formatted as `YYYY-MM-DD`, and optional tree and buffer metadata are
included when supplied.

## Usage

``` r
ltm_cache_file_name(
  lat,
  lon,
  start_date,
  end_date,
  spi,
  proc_level,
  tree_id = NULL,
  buffer_radius_m = NULL
)
```

## Arguments

- lat:

  Numeric latitude used in the request.

- lon:

  Numeric longitude used in the request.

- start_date:

  Start date of the request; coercible to `Date`.

- end_date:

  End date of the request; coercible to `Date`.

- spi:

  Character scalar spectral-index label.

- proc_level:

  Character scalar Sentinel-2 processing-level label.

- tree_id:

  Optional tree identifier included in the cache name.

- buffer_radius_m:

  Optional buffer radius in meters included in the cache name.

## Value

Character scalar path ending in `.rds` under
[`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md).

## See also

Other cache and configuration helpers:
[`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md),
[`ltm_check_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_cache.md),
[`ltm_config_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md),
[`ltm_save_to_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_save_to_cache.md)
