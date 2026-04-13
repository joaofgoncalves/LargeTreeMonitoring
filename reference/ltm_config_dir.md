# Get the user configuration directory

Returns the platform-specific user configuration directory used by
LargeTreeMonitoring and creates it if needed. The location is resolved
with [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) using
`which = "config"`.

## Usage

``` r
ltm_config_dir()
```

## Value

Normalized character scalar path to the package configuration directory.

## See also

Other cache and configuration helpers:
[`ltm_cache_dir()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_dir.md),
[`ltm_cache_file_name()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_file_name.md),
[`ltm_check_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_cache.md),
[`ltm_save_to_cache()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_save_to_cache.md)
