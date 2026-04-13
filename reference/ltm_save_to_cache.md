# Save an `spidf` object to the package cache

Writes a spectral-index data frame to an RDS file in the cache
directory. The cache file name is derived either from `metadata_tags` or
from metadata attributes stored on `spidf_ts`.

## Usage

``` r
ltm_save_to_cache(spidf_ts, metadata_tags = NULL)
```

## Arguments

- spidf_ts:

  Object to cache, normally an object of class `spidf`.

- metadata_tags:

  Optional list containing `lat`, `lon`, `start_date`, `end_date`,
  `spi`, `proc_level`, `tree_id`, and `buffer_radius_m`. When omitted,
  those values are read from attributes on `spidf_ts`.

## Value

Invisibly returns the character scalar path to the written cache file.

## See also

Other cache and configuration helpers:
[`ltm_cache_dir()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md),
[`ltm_cache_file_name()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_file_name.md),
[`ltm_check_cache()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_cache.md),
[`ltm_config_dir()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md)
