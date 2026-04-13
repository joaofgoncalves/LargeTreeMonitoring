# Get a Sentinel-2 image collection from Google Earth Engine

Returns a harmonized Sentinel-2 image collection from Google Earth
Engine for the requested processing level and renames the selected bands
to standardized names used by the package.

## Usage

``` r
ltm_get_s2_imgcol(proc_level = "L2A")
```

## Arguments

- proc_level:

  Character. Sentinel-2 processing level to use. Supported values are
  `"L2A"` or `"L2"` for surface reflectance, and `"L1C"` or `"L1"` for
  top-of-atmosphere reflectance.

## Value

An `ee$ImageCollection` with selected and renamed Sentinel-2 bands.
