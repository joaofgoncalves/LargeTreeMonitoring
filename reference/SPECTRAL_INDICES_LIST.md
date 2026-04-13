# Supported Sentinel-2 spectral indices

Character vector containing the spectral index names accepted by
[`ltm_s2_get_data_point()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md)
and related workflows. The package currently supports normalized
difference vegetation index (`"NDVI"`), enhanced vegetation index
(`"EVI"`), normalized burn ratio (`"NBR"`), normalized difference
red-edge index (`"NDRE"`), and two-band enhanced vegetation index
(`"EVI2"`).

## Usage

``` r
SPECTRAL_INDICES_LIST
```

## Format

A character vector.

## See also

Other package constants:
[`PROC_LEVELS_LIST`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/PROC_LEVELS_LIST.md),
[`VALID_DATA_TYPES`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/VALID_DATA_TYPES.md)
