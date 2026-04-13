# Compute NDVI and attach a binary cloud mask

Computes the normalized difference vegetation index (NDVI) from the
`NIR` and `Red` bands of a Sentinel-2 image and adds a binary cloud mask
derived from the `QA60` band. The output preserves the original image
properties and acquisition time.

## Usage

``` r
ltm_calc_ndvi(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with two bands: `NDVI` and `cloud_mask`.
