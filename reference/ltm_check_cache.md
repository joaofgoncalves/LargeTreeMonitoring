# Look up a cached Sentinel-2 request

Searches the current cache location and legacy fallback names for a
cached `spidf` object matching a Sentinel-2 request. The lookup
tolerates older file names that did not encode optional tree identifiers
or buffer radii.

## Usage

``` r
ltm_check_cache(
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

Character scalar path to the first matching cache file, or `NULL` when
no cache file is found.

## See also

Other cache and configuration helpers:
[`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md),
[`ltm_cache_file_name()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_file_name.md),
[`ltm_config_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md),
[`ltm_save_to_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_save_to_cache.md)
