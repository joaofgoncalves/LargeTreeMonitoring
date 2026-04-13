# Get the cache directory

Resolves the writable cache directory used by LargeTreeMonitoring.
Candidate locations are checked in order: option
`LargeTreeMonitoring.cache_dir`, environment variable `LTM_CACHE_DIR`,
the platform-specific R user cache directory, and the package's legacy
`ltm_cache` directory under the current working directory.

## Usage

``` r
ltm_cache_dir()
```

## Value

Normalized character scalar path to a writable cache directory.

## See also

Other cache and configuration helpers:
[`ltm_cache_file_name()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_cache_file_name.md),
[`ltm_check_cache()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_check_cache.md),
[`ltm_config_dir()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_config_dir.md),
[`ltm_save_to_cache()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_save_to_cache.md)
