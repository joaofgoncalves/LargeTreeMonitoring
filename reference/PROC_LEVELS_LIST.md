# Supported Sentinel-2 processing levels

Character vector containing the Sentinel-2 processing-level labels
accepted by
[`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md).
`"L1"` and `"L1C"` select top-of-atmosphere reflectance, while `"L2"`
and `"L2A"` select surface reflectance.

## Usage

``` r
PROC_LEVELS_LIST
```

## Format

A character vector.

## See also

Other package constants:
[`SPECTRAL_INDICES_LIST`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/SPECTRAL_INDICES_LIST.md),
[`VALID_DATA_TYPES`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/VALID_DATA_TYPES.md)
